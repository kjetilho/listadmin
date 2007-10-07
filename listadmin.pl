#! /usr/bin/perl -w
#
# listadmin - process messages held by Mailman for approval
# Written 2003 - 2007 by
# Kjetil Torgrim Homme <kjetilho+listadmin@ifi.uio.no>
#
# Thank you, Sam Watkins and Bernie Hoeneisen, for contributions and
# feedback.
#
# Released into public domain.

my $version = "2.40";
my $maintainer = "kjetilho+listadmin\@ifi.uio.no";

use HTML::TokeParser;
use LWP::UserAgent;
# use LWP::Debug qw(+trace);
use MIME::Base64;
use MIME::QuotedPrint;
use Data::Dumper;
use Term::ReadLine;
use Getopt::Long;
use Text::Reform;
use I18N::Langinfo qw(langinfo CODESET); # appeared in Perl 5.7.2
use Encode; # appeared in perl 5.7.1
use strict;
use English;

my $rc = $ENV{"HOME"}."/.listadmin.ini";

sub usage {
    my ($exit_val) = @_;
    print STDERR <<_end_;
Usage: $0 [-f CONFIGFILE] [-t MINUTES] [{-a|-r} FILE] [-l] [LISTNAME]
  -f CONFIGFILE    Read configuration from CONFIGFILE.
                   (default: $rc)
  -t MINUTES       Stop processing after MINUTES minutes.  Decimals are
                   allowed.
  --mail           Turn off "nomail" flag for the specified addresses
  --nomail         Turn on "nomail" flag for the specified addresses
  -a FILE          Add e-mail addresses in FILE to list
  -r FILE          Remove e-mail addresses in FILE to list
  --add-member ADDRESS
                   Add ADDRESS as member to list
  --remove-member ADDRESS
                   Remove ADDRESS from member list
  -l               List subscribers
  LISTNAME         Only process lists with name matching LISTNAME.

If options which modify members are used, LISTNAME must match exactly
one list.
_end_
    exit(defined $exit_val ? $exit_val : 64);
}

my ($opt_help, $opt_version, $opt_f, $opt_t, $opt_a, $opt_r,
    @opt_add_member, @opt_remove_member, $opt_l);
my $opt_mail = 1;

GetOptions("help|?" => \$opt_help,
	   "version|V" => \$opt_version,
           "f=s" => \$opt_f,
	   "t=i" => \$opt_t,
	   "mail!" => \$opt_mail,
	   "a=s" => \$opt_a,
	   "r=s" => \$opt_r,
	   "add-member=s" => \@opt_add_member,
	   "remove-member=s" => \@opt_remove_member,
	   "l" => \$opt_l)
	or usage();

usage(0) if $opt_help;
if ($opt_version) {
    print "listadmin version $version\n";
    exit(0);
}

$rc = $opt_f if $opt_f;
usage() if defined $opt_t && $opt_t !~ /\d/ && $opt_t !~ /^\d*(\.\d*)?$/;

push(@opt_add_member, read_address_file($opt_a, 1)) if defined $opt_a;
push(@opt_remove_member, read_address_file($opt_r, 1)) if defined $opt_r;

my $will_modify_membership = 0;
++$will_modify_membership if @opt_add_member;
++$will_modify_membership if @opt_remove_member;

usage() if $will_modify_membership > 1;
usage() if defined $opt_l && $will_modify_membership;

my $ua = new LWP::UserAgent("timeout" => 900, "env_proxy" => 1);
my $time_limit = time + 60 * ($opt_t || 24*60);
my $term;
my $term_encoding = langinfo(CODESET());

# the C and POSIX locale in Solaris uses the charset "646", but Perl
# doesn't support it.
$term_encoding = "ascii" if $term_encoding eq "646";
binmode STDOUT, ":encoding($term_encoding)";
# Turn on autoflush on STDOUT
$| = 1;

my $config = read_config ($rc);
unless ($config) {
    exit (0) unless prompt_for_config ($rc);
    $config = read_config ($rc);
}

my @lists = ();
if (@ARGV) {
    if (defined $config->{$ARGV[0]}) {
	push @lists, $ARGV[0];
    } else {
	@lists = sort config_order grep { /$ARGV[0]/o } keys %{$config}
    }
    if (@lists == 0) {
	print STDERR "$ARGV[0]: no matching list\n";
	usage();
    }
} else {
    @lists = sort config_order keys %{$config}
}

if (@lists > 1 && ($will_modify_membership || defined $opt_l)) {
    print STDERR "Too many matching lists\n";
    print Dumper(\@lists);
    usage();
}

my $list = $lists[0];

my $subscribe_result;
if (@opt_add_member) {
    $subscribe_result = add_subscribers($list, $config->{$list}, $opt_mail,
					@opt_add_member);
}
if (@opt_remove_member) {
    $subscribe_result = remove_subscribers($list, $config->{$list},
					   @opt_remove_member);
}
if (defined $subscribe_result) {
    for my $addr (keys %{$subscribe_result}) {
	print STDERR "$addr: $subscribe_result->{$addr}\n";
    }
    if (%{$subscribe_result}) {
	exit(1);
    } else {
	print "Ok\n";
	exit(0);
    }
}
if (defined $opt_l) {
    my @subscribers = list_subscribers($list, $config->{$list});
    print join("\n", @subscribers, "");
    exit(@subscribers == 0);
}

my ($num, $count, $from, $subject, $reason, $spamscore);


for (@lists) {
    $list = $_;
    my $user = $config->{$list}{"user"};
    my $pw = $config->{$list}{"password"} || "";

    if (time > $time_limit) {
	print "Time's up, skipping the remaining lists\n";
	last;
    }

    my $info = {};
    my $tries = 0;
    print "fetching data for $list ... ";
    do {
	if (-t && ($pw eq "" || $info->{'autherror'})) {
	    print "\n" unless $tries++;
	    $pw = prompt_password("Enter password" .
				  ($user ? " for $user: ": ": "));
	    next if $pw eq "";
	}
	$info = get_list($list, $config->{$list}, $pw);
	if ($info->{'autherror'}) {
	    print "\n" unless $tries++;
	    print STDERR "ERROR: Username or password for $list incorrect\n";
	}
    } while (-t && $info->{'autherror'} && $tries < 9);

    if ($info->{'servererror'}) {
	print "\n";
	printf STDERR ("ERROR: fetching %s\n", $info->{'url'});
	printf STDERR ("ERROR: %s -- skipping list\n",
		       $info->{'servererror'});
	next;
    } elsif ($info->{'autherror'}) {
	print "giving up, proceeding to next list\n";
	next;
    } elsif (! %{$info}) {
	print "nothing in queue\n";
	next;
    } else {
	print "\n";
    }
    $config->{$list}{"password"} = $pw;

    my %change = ();

    process_subscriptions ($info, $config->{$list}, \%change);
    $num = undef;
 restart_approval:
    approve_messages ($info, $config->{$list}, \%change);

    if ($config->{$list}->{"confirm"}) {
	if (scalar %change) {
	redo_confirm:
	    my $c = prompt ("Submit changes? [yes] ");
	    if ($c =~ /^\s*(\?+|h|hj?elp)\s*$/i) {
		print <<_END_;
Nothing will be done to the messages in the administrative queue
unless you answer this question affirmatively.  If you answer "no",
your changes will be discarded and listadmin will proceed to the
next mailing list.  Type "undo" to go back to the current list.
_END_
		goto redo_confirm;
	    }
	    if ($c =~ /^\s*(no?|nei|skip)\s*$/i) {
		print "skipping ...\n";
		next;
	    } elsif ($c =~ /^\d+$/) {
		$num = $c - 1;
		goto restart_approval;
	    } elsif ($c =~ /^u(ndo)?/) {
		--$num;
		goto restart_approval;
	    } elsif ($c !~ /^\s*(|ja?|y|yes)\s*$/i) {
		goto redo_confirm;
	    }
	}
    }
    print "\n";

    commit_changes ($list, $user, $pw, $config->{$list}{"adminurl"},
		    \%change, $info, $config->{$list}{"logfile"});
}

sub process_subscriptions {
    my ($info, $config, $change) = @_;
    my %subscribers = ();
    my $num = 0;
    for my $req (keys %{$info}) {
	if (exists $info->{$req}->{"subscription"}) {
	    $subscribers{$req} = $info->{$req}->{"subscription"};
	    delete $info->{$req};
	}
    }
    my $count = keys (%subscribers);
    my $def = $config->{"subdefault"};
    my $prompt = 'Accept/Reject/Skip/Quit';
    $prompt .= " [" . uc($def) . "]" if $def;
    $prompt .= " ? ";

 subscr_loop:
    for my $id (sort keys %subscribers) {
	last if time > $time_limit;
	++$num;
	print "\n[$num/$count] ========== $list ==========\n";
	print "From:    $subscribers{$id}\n";
	print "         subscription request\n";
	my $ans;
	while (1) {
	    $ans = $config->{"subaction"};
	    $ans ||= prompt ($prompt);
	    $ans = "q" unless defined $ans;
	    $ans =~ s/\s+//g;
	    $ans = $def if $ans eq "";
	    $ans = lc ($ans);
	    if ($ans eq "q") {
		last subscr_loop;
	    } elsif ($ans eq "s") {
		delete $change->{$id};
		next subscr_loop;
	    } elsif ($ans eq "a") {
		$change->{$id} = [ "sa" ];
		last;
	    } elsif ($ans eq "r") {
		my $r = prompt ("Why do you reject? [optional] ");
		unless (defined $r) {

		}
		$change->{$id} = [ "sr", $r ];
		last;
	    } else {
		print STDERR <<"_end_";
Choose one of the following actions by typing the corresponding letter
and pressing Return.

  a  Accept    -- allow the user to join the mailing list
  r  Reject    -- notify sender that the request was turned down
  s  Skip      -- do not decide now, leave it for later
  q  Quit      -- go on to approving messages

_end_
            }
	}
    }
}

sub approve_messages {
    my ($info, $config, $change) = @_;

    my $listdef = $config->{"default"};
    my $spamlevel = $config->{"spamlevel"};
    my $ns_from = $config->{"not_spam_if_from"};
    my $ns_subj = $config->{"not_spam_if_subject"};
    my $dis_from = $config->{"discard_if_from"};
    my $dis_subj = $config->{"discard_if_subject"};
    my $dis_reas = $config->{"discard_if_reason"};

    $count = keys (%{$info}) - 1;	# subtract 1 for globals
    my $search_pattern = "";
    my $dont_skip_forward = 0;
    if (!defined ($num)) {
	$num = 0;
    } else {
	$dont_skip_forward = 1;
    }
    my $tmpl_header = << '_end_';

<<<<<<<<<<<<<<<<<<<< <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
_end_
    my $tmpl_message = << '_end_';
From:     <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
<<<<<<<<  [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
Reason:   <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Spam? <<<
_end_

    my $prompt = 'Approve/Reject/Discard/Skip/view Body/Full/jump #/Undo/Help/Quit';
    my @num_to_id = grep { ! /^global$/ } sort keys %{$info};
    my @undo_list = ();
 msgloop:
    while ($num < $count) {
	last if time > $time_limit;
	my $id = $num_to_id[$num++];
	$from = $info->{$id}{"from"};
	$subject = $info->{$id}{"subject"} || "";
	$reason = $info->{$id}{"reason"};
	$spamscore = $info->{$id}{"spamscore"};
	{
	    # Get rid of warning from Encode:
	    # "\x{516b}" does not map to iso-8859-1 at listadmin.pl line 261.
	    # when run in non UTF-8 environment.
	redraw:
	    local $SIG{__WARN__} = sub {};
	    print form({filler => {left => "=", right => "="}},
		       $tmpl_header,
		       "[$num/$count] =", "$list =");
	    print form({interleave => 1},
		       $tmpl_message,
		       $from,
		       "Subject:", $subject, $reason, $spamscore);
	}
	while (1) {
	    my $ans;
	    my $match = "";
	    if ($spamlevel && $spamscore >= $spamlevel) {
		$match = "spam"; $ans = "d";
	    }
	    $ans ||= $config->{"action"};
	    $match = "From" if got_match ($from, $dis_from);
	    $match = "Subject"
		    if $dis_subj && got_match ($subject, $dis_subj);
	    $match = "reason"
		    if $dis_reas && got_match ($reason, $dis_reas);
	    $ans ||= "d" if $match;
	    $ans = undef if (($ns_subj && $subject =~ $ns_subj) ||
			     ($ns_from && $from =~ $ns_from) ||
			     $dont_skip_forward);

	    if ($ans && $match) {
		if ($match eq "spam") {
		    print "Automatically discarded as spam.\n";
		} else {
		    print "Automatically discarded due to matching $match\n";
		}
		$ans = "d";
	    }
	    my $def = $listdef;
	    $def = $change->{$id}->[0]
		    if defined $change->{$id};
	    my $pr = $prompt;
	    $pr .= " [" . uc($def) . "]" if $def;
	    $pr .= " ? ";
	    $ans ||= prompt ($pr);
	    $ans = "q" unless defined $ans;
	    $ans =~ s/^\s+//;
	    $ans =~ s/\s+$//;
	    $ans = $def if $ans eq "" && defined $def;
	    $ans = lc $ans;
	    if ($ans eq "q") {
		last msgloop;
	    } elsif ($ans eq "s") {
		# Undo will be a no-op, except it will go back to this message.
		push(@undo_list, [$num]);
		delete $change->{$id};
		$dont_skip_forward = 0;
		next msgloop;
	    } elsif ($ans =~ /^\d+$/ && $ans > 0 && $ans <= $count) {
		$num = $ans - 1;
		$dont_skip_forward = 1;
		next msgloop;
	    } elsif ($ans eq "a" || $ans eq "d") {
		# If it is automatically discarded, add it to existing list
		push(@undo_list, []) unless $match && @undo_list;
		push(@{$undo_list[$#undo_list]}, $num);
		$change->{$id} = [ $ans ];
		$dont_skip_forward = 0;
		last;
	    } elsif ($ans eq "u") {
		unless (@undo_list) {
		    print "Nothing to undo.\n";
		    next;
		}
		my @trans_list = @{pop(@undo_list)};
		for my $m (@trans_list) {
		    delete $change->{$num_to_id[$m - 1]};
		}
		$num = $trans_list[0] - 1;
		$dont_skip_forward = 1;
		next msgloop;
	    } elsif ($ans =~ /^list(\s+|$)/) {
		my @list = list_subscribers($list, $config);
		my $member_count = scalar @list;
		if ($POSTMATCH ne "") {
		    @list = grep { /$POSTMATCH/ } @list;
		    printf("Found %d matching addresses:\n  ", scalar @list);
		} else {
		    print "Mailing list members:\n  ";
		}
		print join("\n  ", @list);
		print "\n$member_count members in total\n";
	    } elsif ($ans =~ /^(add|nomail)(\s+|$)/) {
		my $mail = $1 eq "add";
		my $addr = $POSTMATCH || $from;
		my $res = add_subscribers($list, $config, $mail, $addr);
		for my $addr (keys %{$res}) {
		    print "$addr: $res->{$addr}\n";
		}
		print "done\n";
	    } elsif ($ans =~ /^rem(\s+|$)/) {
		my $address = $POSTMATCH;
		my $c = prompt ("Remove subscriber? (there is no undo!) [no] ");
		if ($c =~ /^\s*(ja?|y|yes)\s*$/i) {
		    print "removing...\n";
		    my $res = remove_subscribers($list, $config, $address);
		    for my $addr (keys %{$res}) {
			print "$addr: $res->{$addr}\n";
		    }
		    print "done\n";
		} else {
		    print "aborted\n";
		    next;
		}
	    } elsif ($ans =~ m,([/?])(.*),) {
		my $i = $num - 1;
		my $direction = 1;
		my $fencepost = $count - 1;
		if ($1 eq "?") {
		    $direction = -1;
		    $fencepost = 1;
		}
		# If no pattern is specified, reuse previous pattern.
		$search_pattern = $2 unless $2 eq "";
		if ($search_pattern eq "") {
		    print "No search pattern specified.  Try 'help'\n";
		    next;
		}
		while ($i != $fencepost) {
		    $i += $direction;
		    my $id = $num_to_id[$i];
		    my $search_from = $info->{$id}{"from"};
		    my $search_subject = $info->{$id}{"subject"} || "";
		    if ($search_from =~ /$search_pattern/i ||
			$search_subject =~ /$search_pattern/i) {
			$num = $i;
			$dont_skip_forward = 1;
			next msgloop;
		    }
		}
		print "Pattern not found\n"
	    } elsif ($ans eq "r") {
	    redo_reject:
		my $def_reason = $info->{$id}{"rejreason"};
		$def_reason = $change->{$id}->[1]
			if defined $change->{$id} && $change->{$id}->[0] eq "r";
		my $r = prompt ("Why do you reject? ", $def_reason);
		if ($r =~ /^\s*$/) {
		    print "aborted\n";
		    next;
		} elsif ($r =~ /^\s*(\?+|h|help)\s*$/i) {
		    print "The reason entered will be included in the e-mail ".
			    "sent to the submitter.\n";
		    goto redo_reject;
		}

		push(@undo_list, [ $num ]);
		$change->{$id} = [ "r", $r ];
		$dont_skip_forward = 0;
		last;
	    } elsif ($ans eq "f") {
		# Since the raw bytes aren't really Unicode, we set
		# the replacement sequence to be "<?>" unconditionally.
		print degrade_charset($info->{$id}{"headers"} . "\n\n" .
				      $info->{$id}{"body"}, "questionmark");
	    } elsif ($ans eq "b") {
		my $head = $info->{$id}{"headers"};
		my $text = $info->{$id}{"body"};
		my $mime_headers = "";
		if ($head =~ m,content-type:\s*text/,i) {
		    $mime_headers = $head;
		} elsif ($head =~ m,content-type:\s*multipart/,i) {
		    # This is quick and dirty, we look at the first
		    # MIME headers in the body instead.  We can't do
		    # proper MIME parsing since the message is
		    # truncated by Mailman.
		    $mime_headers = $text;
		}
		if ($mime_headers =~ /content-transfer-encoding:\s+(\S+)/i) {
		    my $cte = $1;
		    if ($cte =~ /quoted-printable/i) {
			$text = MIME::QuotedPrint::decode($text);
		    } elsif ($cte =~ /base64/i) {
			# Don't bother with truncated lines.
			$text =~ s!([A-Za-z0-9/+=]{72,76})!MIME::Base64::decode_base64($1)!ge;
		    }
		}
		if ($mime_headers =~ /charset=(\S+)/i) {
		    my $charset = $1;
		    $charset =~ s/;$//;
		    $charset =~ s/^"(.*)"$/$1/;
		    $charset = guess_charset($charset, $text);
		    eval { $text = Encode::decode($charset, $text) };
		}

		$text = degrade_charset($text, $config->{unprintable});
		my @lines = split (/\n/, $text, 21);
		pop @lines;
		# local $SIG{__WARN__} = sub {}; # see comment elsewhere
		print join ("\n", @lines), "\n";
	    } elsif ($ans eq "t") {
		print $info->{$id}{"date"}, "\n";
	    } elsif ($ans eq "url") {
		print mailman_url($list, $config->{adminurl}), "\n";
	    } elsif ($ans eq ".") {
		goto redraw;
	    } elsif ($ans eq "") {
		# nothing.
	    } else {
		print <<"end";
Choose one of the following actions by typing the corresponding letter
and pressing Return.

  a  Approve     -- the message will be sent to all members of the list
  r  Reject      -- notify sender that the message was rejected
  d  Discard     -- throw message away, don't notify sender
  s  Skip        -- don't decide now, leave it for later
  b  view Body   -- display the first 20 lines of the message
  f  view Full   -- display the complete message, including headers
  t  view Time   -- display the date the message was sent
  #  jump        -- jump backward or forward to message number #
  u  Undo        -- undo last approve or discard
  /pattern       -- search for next message with matching From or Subject
  ?pattern       -- search for previous message with matching From or Subject
  .              -- redisplay entry
  add [address]  -- add subscription for address (defaults to From)
  nomail [address] -- add nomail subscription for address (defaults to From)
  list [pattern] -- list mailing list members matching optional pattern
  rem address    -- remove list member
  q  Quit        -- go on to the next list

end
		print <<"end" if $listdef;
The default action for this list when you only press Return is '$listdef'

end
            }
	}
    }
}

sub url_quote_parameter {
    my $param = shift;
    $param =~ s/(\W)/sprintf ("%%%02x", ord ($1))/ge;
    $param;
}

sub mailman_params {
    my ($user, $pw) = @_;
    my %params;
    $params{"username"} = $user if defined $user;
    $params{"adminpw"} = $pw if defined $pw;
    return \%params;
}

sub uio_adminurl {
    my ($domain) = @_;
    return 'https://{domain}/mailman/{domain}/admindb/{list}'
	    if ($domain eq 'lister.ping.uio.no');
    return 'http://{domain}/mailman/admindb/{list}@{domain}'
	    if ($domain eq "lister.uio.no");
    return 'http://{subdomain}-lists.uio.no/mailman/admindb/{list}@{domain}'
	    if ($domain =~ /^(\w+\.)?uio\.no$/);
    return 'http://lists.{domain}/mailman/admindb/{list}@{domain}'
	    if ($domain eq "simula.no");
    undef;
}

sub mailman_url {
    my ($list, $pattern, $params, $action) = @_;

    my ($lp, $domain) = split ('@', $list);

    $pattern ||= uio_adminurl ($domain);
    $pattern ||= 'http://{domain}/mailman/admindb/{list}';

    my $url = $pattern;
    my $subdom = $domain;
    $subdom = $PREMATCH if $subdom =~ /\./;
    $url =~ s/\{list\}/$lp/g;
    $url =~ s/\{domain\}/$domain/g;
    $url =~ s/\{subdomain\}/$subdom/g;
    if ($action) {
	$url =~ s,/admindb/,/admin/,;
	$url .= "/$action";
    }
    $url .= "?$params" if $params;
    return $url;
}

# Returns a ref to a hash with all the information about pending messages
sub get_list {
    my ($list, $config, $pw) = @_;

    my $starttime = time;
    my $mmver;
    my ($page, $page_appr, $resp_appr);
    my $url = mailman_url($list, $config->{"adminurl"});
    my $resp = $ua->post($url, mailman_params($config->{"user"}, $pw));
    unless ($resp->is_success) {
	return {'servererror' => $resp->status_line, 'url' => $url};
    }
    $page = $resp->content;

    my $dumpdir = $config->{"dumpdir"};
    my $dumpfile;
    if ($dumpdir && $page) {
	$dumpfile = "$dumpdir/dump-$list.html";
	if (open (DUMP, ">$dumpfile")) {
	    print DUMP $page;
	    close (DUMP);
	}
    }

    if ($page eq "") {
	if (time - $starttime > 60) {
	    return {servererror => "Mailman server timed out?", url => $url};
	} else {
	    return {servererror => "Empty page", url => $url};
	}
    } elsif ($page =~ get_trans_re("no_such_list")) {
	return {servererror => "No such list", url => $url}
    }

    my $parse = HTML::TokeParser->new(\$page) || die;
    $parse->get_tag ("title") || die;
    my $title = $parse->get_trimmed_text ("/title") || die;

    if ($title =~ get_trans_re("authentication")) {
	return {'autherror' => 1};
    }

    if ($page !~ get_trans_re("pending_req")) {
	my $msg = "unexpected contents";
	# Use rand() to protect a little against tmpfile races
	$dumpfile ||= "/tmp/dump-" . rand() . "-$list.html";
	if (open(DUMP, ">$dumpfile")) {
	    chmod(0600, $dumpfile);
	    print DUMP $page;
	    close(DUMP);
	    $msg .= ", please send $dumpfile to $maintainer";
	}
	return {servererror => $msg, url => $url};
    }

    my @mailman_mentions = grep {/Mailman/} split (/\n/, $page);
    for my $mention (reverse @mailman_mentions) {
	if ($mention =~ /\bv(ersion)?\s(\d+\.\d+)/) {
	    $mmver = $2;
	    last;
	}
    }
    unless ($mmver) {
	die "Can not find version information, please mail maintainer.";
    }

    if ($mmver ge "2.1") {
	# Mailman does not look for "details" in parameters, so it
	# must be part of the query string.
	$url = mailman_url($list, $config->{"adminurl"}, "details=all");
	$resp = $ua->post($url, mailman_params($config->{"user"}, $pw));
	unless ($resp->is_success) {
	    return {'servererror' => $resp->status_line, 'url' => $url};
	}
	$page_appr = $resp->content;
	if (defined $dumpdir &&
	    open (DUMP, ">$dumpdir/dump-details-$list.html")) {
	    print DUMP $page_appr;
	    close (DUMP);
	}
    }

    my $data;
    if ($mmver eq "2.1") {
	my $parse_appr = HTML::TokeParser->new(\$page_appr) || die;
	$data = parse_pages_mm_2_1($mmver, $config, $parse, $parse_appr);
    } else {
	$data = parse_pages_mm_old($mmver, $config, $parse);
    }
    set_param_values($mmver, $data) if %{$data};
    return $data;
}

sub parse_pages_mm_old {
    my ($mmver, $config, $parse) = @_;

    my %data = ();
    my $token;
    $parse->get_tag ("hr");
    $parse->get_tag ("h2") || return \%data;
    my $headline = $parse->get_trimmed_text ("/h2") || die;
    if ($headline =~ get_trans_re("headline_subscr")) {
	parse_subscriptions ($mmver, $config, $parse, \%data);
	$token = $parse->get_token;
	if (lc ($token->[1]) eq "input") {
	    return (\%data);
	} else {
	    $parse->get_tag ("h2") || die;
	    $headline = $parse->get_trimmed_text ("/h2") || die;
	}
    }
    if ($headline =~ get_trans_re("held_for_approval")) {
	parse_approvals ($mmver, $config, $parse, \%data);
    } else {
	$parse->get_tag ("hr") || die;
	$token = $parse->get_token;
	if ($token->[0] eq "S" && lc ($token->[1]) eq "center") {
	    parse_approvals ($mmver, $config, $parse, \%data);
	}
    }
    return (\%data);
}

sub parse_pages_mm_2_1 {
    my ($mmver, $config, $parse_subs, $parse_appr) = @_;

    my %data = ();
    my $headline;

    $parse_subs->get_tag ("hr");
    if ($parse_subs->get_tag ("h2")) {
	parse_subscriptions ($mmver, $config, $parse_subs, \%data);
    }

    $parse_appr->get_tag ("hr");
    if ($parse_appr->get_tag ("h2")) {
	parse_approvals ($mmver, $config, $parse_appr, \%data);
    }
    return (\%data);
}

sub parse_subscriptions {
    my ($mmver, $config, $parse, $data) = @_;
    my $token;

    $parse->get_tag ("table") || die;
    $parse->get_tag ("tr") || die;
    $parse->get_tag ("tr") || die;
    do {
	parse_subscription ($mmver, $config, $parse, $data);
	do {
	    $token = $parse->get_token;
	} until ($token->[0] eq "S");
    } while (lc ($token->[1]) eq "tr");
}

sub parse_subscription {
    my ($mmver, $config, $parse, $data) = @_;

    $parse->get_tag ("td") || die;
    my $address = $parse->get_trimmed_text ("/td") || die;
    my $tag = $parse->get_tag ("input") || die;
    my $id = $tag->[1]{"name"};
    $parse->get_tag ("/table") || die;
    $parse->get_tag ("/tr") || die;
    $data->{$id} = { "subscription" => $address };
}

sub parse_approvals {
    my ($mmver, $config, $parse, $data) = @_;
    my $token;

    do {
	$parse->get_tag ("table") || die;
	parse_approval ($mmver, $config, $parse, $data);
	$parse->get_tag ("/table");
	$parse->get_tag ("hr");
	$token = $parse->get_token;
	$token = $parse->get_token
		if ($token->[0] eq "S" && lc ($token->[1]) eq "center");
    } until ($token->[0] eq "S" && lc ($token->[1]) eq "input");
}

sub get_trans_re {
    my ($key) = @_;

    # Handle translations -- poorly...
    #
    # For now, we look for strings in all languages at the same time
    # since they don't seem to overlap.  This might have to change
    # later.
    #
    # Please send additions if you have them.

    my %translations =
	    ("authentication" =>
	     {
	      "en" => "authentication",
	      "de" => "Authentifikation",
	      "fr" => "authentification",
	     },
	     "subscr_success" =>
	     {
	      "en" => "Successfully ((un)?subscribed|Removed)",
	      "de" => "Erfolgreich (ein|aus)getragen",
	     },
	     "subscr_error" =>
	     {
	      "en" => "Error (un)?subscribing",
	     },
	     "no_such_list" =>
	     {
	      "en" => "Mailman Admindb Error.*No such list:",
	     },
	     "pending_req" =>
	     {
	      "en" => "(current set of administrative|pending request)",
	      "de" => "(gegenw&auml;rtigen administrativen|unbearbeiteten Anfragen)",
	     },
	     "headline_subscr" =>
	     {
	      "en" => "subscription",
	     },
	     "held_for_approval" =>
	     {
	      "en" => "held for approval",
	     },
	     "already_member" =>
	     {
	      "en" => "Already a member",
	     },
	    );

    my $t = $translations{$key};
    die "INTERNAL ERROR: Unknown translation key '$key'\n"
	    unless defined $t;
    return "(?i)(" . join("|", values %{$t}) . ")";
}

sub guess_charset {
    my ($charset, $text) = @_;

    # Mislabeling Shift JIS as ISO 2022 is a very common mistake.
    if ($charset =~ /^iso-2022-jp/i && $text =~ /[\x80-\x9f]/) {
	return "Shift_JIS";
    }
    return $charset;
}

sub decode_rfc2047_qp {
    my ($charset, $encoded_word) = @_;
    my $text = $encoded_word;
    $text =~ s/_/ /g;
    $text = MIME::QuotedPrint::decode($text);
    $charset = guess_charset($charset, $text);
    eval { $text = Encode::decode($charset, $text) };
    return defined $text ? $text : $encoded_word;
}

sub decode_rfc2047_base64 {
    my ($charset, $encoded_word) = @_;
    my $text = MIME::QuotedPrint::decode_base64($encoded_word);
    $charset = guess_charset($charset, $text);
    eval { $text = Encode::decode($charset, $text) };
    return defined $text ? $text : $encoded_word;
}

sub decode_rfc2047 {
    my ($hdr, $config) = @_;

    # Bugs: Decodes invalid tokens, where the encoded word is
    # concatenated with other letters, e.g.  foo=?utf-8?q?=A0=F8?=
    # Also decodes base64 encoded words which are doubly encoded with
    # quoted-printable.

    $hdr =~ s/=\?([^? ]+)\?q\?([^? ]*)\?=/
	    decode_rfc2047_qp($1, $2)/ieg;
    $hdr =~ s/=\?([^? ]+)\?b\?([^? ]*)\?=/
	    decode_rfc2047_base64($1, $2)/ieg;

    return degrade_charset($hdr, $config->{unprintable});
}

sub degrade_charset {
    my ($text, $unprintable) = @_;

    # Handle unencoded Shift JIS (Japanese) text.  The input text is
    # either raw data from the message, or Unicode, in which case it
    # will not contain these code points.  This discrimates slightly
    # against users of Windows-1252, which has curved quotes at 0x82
    # (0x81 is unassigned).

    if ($text =~ /[\x81\x82]/) {
	eval { $text = Encode::decode("Shift_JIS", $text) };
    }

    # This may look a bit silly.  We first encode to the character set
    # of our terminal.  If it is a limited character set such as
    # Latin1, Chinese glyphs are converted into e.g. "&#x41a;", while
    # "n with tilde" will be a single glyph.  We then convert this
    # back to a Unicode string so that the length is right (number of
    # glyphs, not octets) for Text::Reform.  Finally, when the Unicode
    # string is printed to the screen, the binmode directive for
    # STDOUT tells Perl to once more translate it into the terminal's
    # character set.

    eval {
	$text = Encode::decode($term_encoding,
			       Encode::encode($term_encoding, $text,
					      Encode::FB_HTMLCREF))
	    };

    # The built-in formats for unprintable glyphs are ugly, and to be
    # allowed to specify a code ref which returns our preferred format
    # directly, we need to require Encode version 2.10, which feels a
    # bit unnecessary.

    if (defined $config && $unprintable eq "unicode") {
	$text =~ s/&\#(\d+);/sprintf("<U+%04x>", $1)/ge;
    } else {
	$text =~ s/&\#\d+;/<?>/g;
    }

    # Get rid of ESC sequences which may cause havoc with the
    # terminal, we only keep TAB and LF.  Also removes control
    # characters with high bit set, 127-159, which are unallocated in
    # Unicode.

    $text =~ s/([\x00-\x08\x0b-\x1f\x7f-\x9f])/sprintf("<%02x>", ord($1))/eg;

    return $text;
}


sub parse_approval {
    my ($mmver, $config, $parse, $data) = @_;
    my ($from, $reason, $subject, $id, $body, $headers);

    $parse->get_tag ("tr") || die;	# From:
    $parse->get_tag ("td") || die;
    $parse->get_tag ("td") || die;
    $from = $parse->get_trimmed_text("/td");

    if ($mmver eq "1.2") {
	$parse->get_tag ("tr") || die;	# Reason:
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die; 
	$reason = $parse->get_trimmed_text("/td");
	$parse->get_tag ("tr") || die;	# Subject:
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die;
	$subject = $parse->get_trimmed_text("/td");
    } else {
	$parse->get_tag ("tr") || die;	# Subject:
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die; 
	$subject = $parse->get_trimmed_text("/td");
	$parse->get_tag ("tr") || die;	# Reason:
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die;
	$reason = $parse->get_trimmed_text("/td");
    }
    $parse->get_tag ("tr") || die;	# Action:
    my $tag = $parse->get_tag ("input") || die;
    $id = $tag->[1]{"name"};

    $data->{$id} = { "from" => decode_rfc2047($from, $config),
		     "subject" => $subject,
		     "reason" => $reason };

    $parse->get_tag ("tr") || die; # Reject _or_ Preserve message
    if ($mmver ge "2.0") {
	$parse->get_tag ("tr") || die;    # forward
	$parse->get_tag ("tr") || die;    # Reject
    }
    $parse->get_tag ("td") || die;
    $parse->get_tag ("td") || die;
    $data->{$id}->{"rejreason"} = $parse->get_trimmed_text("/td") || die;


    $parse->get_tag ("tr") || die; # Message Excerpt _or_ Headers
    $parse->get_tag ("td") || die;
    $parse->get_tag ("td") || die;
    $headers = $parse->get_text("/td");

    # We handle spam score headers on the formats:
    #   X-spam-score: *****
    #   X-spam-score: 4.23 (****)
    #
    # The name of the header is flexible.
    my $header_re = $config->{"spamheader"} || 'X-\S*spam-?(?:level|score)';

    # Extract all spam score headers, and pick the max value:
    my $spamscore = 0;
    while ($headers =~ /^$header_re:\s+
	                   (-?\d+\.\d+\s+)?
                           \(?
                             ((\S)\3*)
                           (?:\s|\)|$)/xgim) {
	my $score = defined $1 ? int($1): length($2);
	$spamscore = $score if $score > $spamscore;
    }
    $data->{$id}->{"spamscore"} = $spamscore;
    $data->{$id}->{"date"} = "<no date>";
    $data->{$id}->{"date"} = $1
	    if $headers =~ /^Date:\s+(.*)$/m;
    if ($mmver ge "2.0") {
	$parse->get_tag ("tr") || die;  # Message Excerpt
	$parse->get_tag ("td") || die;
	$parse->get_tag ("textarea") || die;
	$body = $parse->get_text("/textarea");
    } else {
	$headers =~ s/\n\n//s;
	$body = $POSTMATCH;
	$headers = $PREMATCH;
    }
    $headers =~ s/\n(\s)/$1/g; # Header folding
    $headers =~ s/^\s+//;
    $data->{$id}->{"headers"} = $headers;

    # Mailman decodes Subject itself, but at least version 2.0 and 2.1
    # screw up non-ASCII characters, so we get the raw value from the
    # headers instead.
    if ($headers =~ /^Subject:\s*(.*)\s*$/mi) {
	$subject = $1;
    }
    if ($subject =~ /[\x80-\xff]/) {
	$subject .= " [unencoded]";
    }

    $data->{$id}->{"subject"} = decode_rfc2047($subject, $config);

    $body .= "\n" unless $body =~ /\n$/;
    $data->{$id}->{"body"} = $body;

    return ($mmver);
}

sub set_param_values {
    my ($mmver, $data) = @_;

    if ($mmver ge "2.0") {
	$data->{"global"}{"actions"} = { "a" => 1,
					 "r" => 2,
					 "d" => 3,
					 "sa" => 4, # subscribe approve
					 "sr" => 2, # subscribe reject
				     };
    } else {
	$data->{"global"}{"actions"} = { "a" => 0,
					 "r" => 1,
					 "d" => 2,
					 "sa" => 1, # subscribe approve
					 "sr" => 0, # subscribe reject
				     };
    }
}

sub read_config {
    my ($file) = @_;

    my %cur = map { $_ => undef; }
	    qw (not_spam_if_from
		not_spam_if_subject
		discard_if_from
		discard_if_subject
		discard_if_reason);
    my $pattern_keywords = join ("|", keys %cur);

    # Defaults:
    $cur{user} = $cur{password} = $cur{action} = $cur{default} = "";
    $cur{confirm} = 1;
    $cur{unprintable} = "questionmark";

    my $conf = {};
    my $line = "";
    my $count = 0;
    my $lineno = 0;

    my %act = ("approve" => "a", "discard" => "d",
	       "reject" => "r", "skip" => "s", "none" => "");
    my %sact = ("accept" => "a",
		"reject" => "r", "skip" => "s", "none" => "");

    return undef unless open (CONF, $file);
    while (<CONF>) {
	++$lineno;
	chomp;
	s/\r$//;
	s/\s+$//;		# trailing whitespace is "always" unintended
	next if /^\s*\#/;
	s/^\s+// if $line;	# remove leading whitespace after continuation
	if (/\\$/) {
	    $line .= $PREMATCH;
	    next;
	}
	$line .= $_;
	$line =~ s/^\s+//;
	next if /^$/;
	if ($line =~ /^username\s+/i) {
	    $cur{user} = unquote($POSTMATCH);
	    if ($cur{user} !~ /^[a-z0-9._+-]+\@[a-z0-9.-]+$/) {
		print STDERR "$file:$lineno: Illegal username: '$cur{user}'\n";
		exit 1;
	    }
	} elsif ($line =~ /^password\s+/i) {
	    $cur{password} = unquote($POSTMATCH);
	} elsif ($line =~ /^spamlevel\s+/i) {
	    $cur{spamlevel} = unquote($POSTMATCH);
	    if ($cur{spamlevel} =~ /^(\d+)\s*$/) {
		$cur{spamlevel} = $1;
	    } else {
		print STDERR "$file:$lineno: Illegal value: '$cur{spamlevel}'\n";
		print STDERR "choose a positive numeric value\n";
		exit 1;
	    }
	} elsif ($line =~ /^(confirm|meta_member_support)\s+/i) {
	    my ($key, $value) = (lc($1), unquote($POSTMATCH));
	    if ($value eq "yes") {
		$value = 1;
	    } elsif ($value eq "no") {
		$value = undef;
	    } else {
		print STDERR "$file:$lineno: Illegal value: '$value\n";
		print STDERR "choose one of yes or no\n";
		exit 1;
	    }
	    $cur{$key} = $value;
	} elsif ($line =~ /^(action|default)\s+/i) {
	    my ($key, $value) = (lc($1), unquote($POSTMATCH));
	    unless (exists $act{$value}) {
		print STDERR "$file:$lineno: Illegal value: '$value\n";
		print STDERR "choose one of ",
                             join (", ", sort keys %act), "\n";
		exit 1;
	    }
	    $cur{$key} = $act{$value};
	} elsif ($line =~ /^adminurl\s+/i) {
	    $cur{adminurl} = unquote($POSTMATCH);
	    $cur{adminurl} = undef if $cur{adminurl} eq "NONE";
	} elsif ($line =~ /^log\s+/i) {
	    $cur{logfile} = expand_pathname(unquote($POSTMATCH));
	} elsif ($line =~ /^dumpdir\s+/i) {
	    $cur{dumpdir} = expand_pathname(unquote($POSTMATCH));
	    mkdir($cur{dumpdir}) if (defined $cur{dumpdir});
	} elsif ($line =~ /^subscription_(action|default)\s+/) {
	    my $key = "sub" . lc($1);
	    my $value = unquote($POSTMATCH);
	    unless (exists $sact{$value}) {
		print STDERR "$file:$lineno: Illegal value: '$value'\n";
		print STDERR "choose one of ",
		             join (", ", sort keys %sact), "\n";
		exit 1;
	    }
	    $cur{$key} = $sact{$value};
	} elsif ($line =~ /^($pattern_keywords)\s+/o) {
	    my $key = $1;
	    my $val = $POSTMATCH;
	    $val =~ s/\s+$//;
	    if ($val =~ /^"(.*)"$/) {
		$val = $1;
		$val =~ s/\\"/"/g;
		$val =~ s/\\\\/\\/g;
	    }
	    $cur{$key} = ($val eq "NONE") ? undef : $val;
	} elsif ($line =~ /^spamheader\s+/) {
	    $cur{spamheader} = unquote($POSTMATCH);
	    unless ($cur{spamheader} =~ /^[\w-]+$/) {
		print STDERR "$file:$lineno: Illegal header name: ".
			"'$cur{spamheader}'\n";
		exit 1;
	    }
	    $cur{spamheader} = undef if $cur{spamheader} eq "default";
	} elsif ($line =~ /^([^@ \t]+@[^@])+\s*/) {
	    my %copy = %cur;
	    $copy{order} = ++$count;
	    $conf->{$line} = \%copy;
	} elsif ($line =~ /^unprintable\s+/) {
	    $cur{unprintable} = unquote($POSTMATCH);
	    unless ($cur{unprintable} =~ /^(questionmark|unicode)$/) {
		print STDERR "$file:$lineno: Illegal format for ".
			"unprintable characters: '$cur{unprintable}'\n";
		exit 1;
	    }
	} else {
	    print STDERR "$file:$lineno: Syntax error: '$line'\n";
	    exit 1;
	}
	$line = "";
    }
    close (CONF);
    return $conf;
}

sub unquote {
    my ($val) = @_;
    $val =~ s/\s+$//;
    if ($val =~ /^"(.*)"$/) {
	$val = $1;
	$val =~ s/\\"/"/g;
	$val =~ s/\\\\/\\/g;
    }
    return ($val);
}

sub expand_pathname {
    my ($pathname) = @_;

    $pathname =~ s,^\$HOME/,$ENV{'HOME'}/,;
    $pathname =~ s,^~/,$ENV{'HOME'}/,;
    $pathname =~ s,^~(\w+)/,(getpwnam($1))[7]."/",e;
    if ($pathname =~ /^M:/i) {
	$pathname =~ s,\\,/,g;
	$pathname =~ s,^M:,$ENV{'HOME'},;
    }
    $pathname = undef if $pathname eq "none";
    return $pathname;
}


sub prompt_for_config {
    my ($rc) = @_;

    print "No configuration file found: $rc\n";
    my $ans = prompt ("Do you want to create one? [yes] ");
    print "\n";
    if ($ans !~ /^\s*(|y|yes|j|ja)\s*$/i) {
	print "I take that as a no.  Goodbye!\n";
	return undef;
    }
    umask 077;
    unless (open (RC, ">$rc")) {
	print STDERR "$rc: $!\n";
	return undef;
    }
    my $user = prompt ("Enter Mailman username: ");
    print "\n";
    print RC "username $user\r\n";
    my $pass = prompt_password("Enter Mailman password: ");
    print "\n";
    $pass =~ s/"/\\"/g;
    print RC "password \"$pass\"\r\n";

    print <<END;
Listadmin can discard messages with a high spam score automatically.
A value in the interval 5 to 12 is recommended.
END
    my $spam = prompt ("What threshold do you want? [8]");
    print "\n";
    $spam =~ s/\s*//g;
    $spam ||= "8";
    if ($spam =~ /^\d+$/) {
	print RC "spamlevel $spam\r\n";
    } else {
	print "No automatic discard will be done.\n";
    }
    my $extra = <<END;

# If you uncomment the following you will only have to press Return
# to discard a message:
#
# default discard

# Uncomment the following to get a terse transaction log:
#
# log "~/.listadmin.log"

END
    $extra =~ s/\n/\r\n/g;
    print RC $extra;

    print <<END;
Now enter the addresses of the lists you maintain.  End with an empty
line.
END
    my $list;
    do {
	$list = prompt ("> ");
	print "\n";
	$list =~ s/\s*//g if $list;
	print RC "$list\r\n" if $list;
    } while ($list);
    close (RC);
    print <<END;

The configuration has been saved in $rc.
You can edit this file with an ordinary text editor, such as Notepad,
Pico, or Emacs.  To read about all the configuration options, run
'man listadmin'.

END
    return 1;
}

sub commit_changes {
    my ($list, $user, $pw, $url, $change, $msgs, $logfile) = @_;

    my $baseurl = mailman_url ($list, $url);
    my $action = $msgs->{"global"}{"actions"};
    my $changes = 0;
    my $update_total = scalar (keys %{$change});
    my $update_count = 0;
    my $params = mailman_params ($user, $pw);

    my $log = log_timestamp ($list);
    # Expand {list}, {subdomain} and {domain}
    $logfile = mailman_url($list, $logfile);

    for my $id (sort { $a <=> $b } keys %{$change}) {
	my ($what, $text) = @{$change->{$id}};
	$params->{$id} = $action->{$what};
	unless ($what =~ /^s[ar]$/) {
	    # we don't log subscription approval or rejects
	    $log .= sprintf ("%s D:[%s] F:[%s] S:[%s]\n",
			     $what,
			     $msgs->{$id}{"date"},
			     $msgs->{$id}{"from"},
			     $msgs->{$id}{"subject"});
	}
	if ($what =~ /^s?r$/) {
	    $params->{"comment-$id"} = $text;
	}
	++$changes;

	# HTTP does not specify a maximum size for a POST request, so
	# we could do this as one request.  However, Apache is usually
	# set up to close the connection after the CGI script has run
	# for 5 minutes, so we reduce the size of each request to be
	# nice to slow servers.

	if ($changes >= 100) {
	    $update_count += $changes;
	    printf("sending %d updates to server, %d left    \r",
		   $changes, $update_total - $update_count);
	    submit_http ($baseurl, $params, $log, $logfile);
	    $log = log_timestamp ($list);
	    $changes = 0;
	    $params = mailman_params ($user, $pw);
	    
	    # even if time has run out, we will always submit at least
	    # one batch of data.
	    if (time > $time_limit) {
		print "\nTime's up, won't submit the other changes\n";
		last;
	    }
	}
    }
    submit_http ($baseurl, $params, $log, $logfile)
	    if $changes;
    print (" " x 72, "\r") if $update_count > 0;
}

sub log_timestamp {
    my $list = shift;

    my ($sec, $min, $hour, $mday, $mon, $year) = (localtime (time))[0..5];
    return (sprintf ("submitting %s %04d-%02d-%02dT%02d:%02d:%02d\n",
		     $list, $year+1900, $mon+1, $mday, $hour, $min, $sec));
}

sub add_subscribers {
    my ($list, $config, $mail, @addresses) = @_;

    die unless @addresses;

    fetch_meta_members($list, $config);

    my %params = (username => $config->{user},
		  adminpw => $config->{password},
		  subscribe_or_invite => 0,                # Mailman 2.x
		  send_notifications_to_list_owner => 0,   # Mailman 2.x
		  send_welcome_message_to_this_batch => 0, # Mailman 2.x
		  send_welcome_msg_to_this_batch => 0,     # Mailman 1.2
		  meta_members => $config->{meta_members}, # Mailman 1.2
		  subscribees => join("\n", @addresses));
    my $url = mailman_url($list, $config->{adminurl}, "", "members");
    my $resp = $ua->post($url, \%params);
    return $resp->status_line unless $resp->is_success;

    my $result = parse_subscribe_response($resp->content);

    if (!$mail) {
	my %left = map { $_ => 1 } @addresses;
	for my $failed (keys %{$result}) {
	    unless ($result->{$failed} =~ get_trans_re("already_member")) {
		delete $left{$failed};
	    }
	}
	@addresses = keys %left;
    } else {
	# We only need to reset "nomail" on the users who already were
	# members.
	@addresses = ();
	for my $failed (keys %{$result}) {
	    if ($result->{$failed} =~ get_trans_re("already_member")) {
		push(@addresses, $failed);
	    }
	}
    }
    if (@addresses) {
	%params = (username => $config->{user},
		   adminpw => $config->{password},
		   user => \@addresses,
		   meta_members => $config->{meta_members}, # Mailman 1.2
		   setmemberopts_btn => "submit");	# Mailman 2.x
	for my $a (@addresses) {
	    $params{$a . "_nomail"} = "on" unless $mail;
	    $params{$a . "_subscribed"} = "on";		# Mailman 1.2
	}
	$resp = $ua->post($url, \%params);
	return $resp->status_line unless $resp->is_success;
    }

    return $result;
}

sub remove_subscribers {
    my ($list, $config, @addresses) = @_;

    fetch_meta_members($list, $config);

    my $url = mailman_url($list, $config->{adminurl}, "", "members");

    # In Mailman 1.2, unsubscription happens when an address is
    # mentioned in "user" without a corresponding
    # "$address_subscribed" parameter
    my %params = (username => $config->{user},
		  adminpw => $config->{password},
		  setmemberopts_btn => "submit",	# Mailman 2.x
		  meta_members => $config->{meta_members}, # Mailman 1.2
		  user => \@addresses);
    for my $a (@addresses) {
	$params{$a . "_unsub"} = "on";			# Mailman 2.x
    }
    my $resp = $ua->post($url, \%params);
    return $resp->status_line unless $resp->is_success;

    return parse_subscribe_response($resp->content);
}


sub parse_subscribe_response {
    my ($page) = @_;

    # Normalise, to make parsing easier (Hack!)
    $page =~ s/<h3\>/\<h5\>/ ;
    $page =~ s/<\/h3\>/\<\/h5\>/;

    # In Mailman 1.2 and 2.0, you will not get an explicit success
    # report when removing subscribers, so we only return the
    # failures since the successes can be inferred anyway.

    my %failure = ();

    my $parse = HTML::TokeParser->new(\$page) || die;

    while ($parse->get_tag ("h5")) {
	my $h5 = $parse->get_text ("/h5");

	$parse->get_tag ("ul") || die;
	my $ul = $parse->get_text ("/ul") || die;

	if ($h5 =~ get_trans_re("subscr_success")) {
	    # hooray!
	} elsif ($h5 =~ get_trans_re("subscr_error")) {
	    for (split(/\n/, $ul)) {
		chomp;
		if (/^\s*(.*?)\s*--\s*(.*)/) {
		    $failure{$1} = $2;
		}
	    }
	} else {
	    $ul =~ s/\n/\n\t/g;
	    print STDERR "You have an unusual Mailman output.  Please mail ".
		    "this message to\n$maintainer\n:\n".
		    "\t[$h5]\n\t[$ul]\nThanks!\n";
	}
	$parse->get_tag ("p") || die;
    }

    return \%failure;
}

sub list_subscribers {
    my ($list, $config) = @_;

    fetch_meta_members($list, $config);
    my $url = mailman_url($list, $config->{adminurl}, "", "members");
    my %params = (username => $config->{user},
		  adminpw => $config->{password},
		  meta_members => $config->{meta_members},
		  chunk => 0);
    my $resp = $ua->post($url, \%params);
    unless ($resp->is_success) {
	print "$url: ", $resp->status_line, "\n";
	return ();
    }

    my @addresses = ();
    my ($parse, $page, $tag);

 member_letter:
    for my $letter ("a" .. "z") {
	my $chunk = 0;

	$params{chunk} = $chunk;

	# Mailman 2.x specifically looks at QUERY_STRING, so chunk and
	# letter can't be parameters to POST.  However, Mailman 1.x
	# only looks at chunk in the POST parameters.
	$resp = $ua->post("$url?letter=$letter&chunk=$chunk", \%params)
		unless $letter eq "a";

	while ($resp->is_success) {
	    $page = $resp->content;
	    $parse = HTML::TokeParser->new(\$page);
	    my $count = 0;
	    my $repeated = 0;
	    my $later_letter = 0;
	    while ($tag = $parse->get_tag("input")) {
		my $attr = $tag->[1];
		if ($attr->{type} =~ /^hidden$/i &&
		    $attr->{name} =~ /^user$/i) {
		    ++$count;
		    my $address = $attr->{value};
		    unless ($address =~ /\@/) {
			# Mailman 2.x adds URL-encoding
			$address =~ s/%([0-9a-fA-F]{2})/sprintf("%c", hex($1))/ge;
		    }
		    ++$later_letter if lc(substr($address, 0, 1)) gt $letter;
		    if (grep { $_ eq $address } @addresses) {
			++$repeated;
		    } else {
			push(@addresses, $address);
		    }
		}
	    }
	    last if $count == 0;

	    # In Mailman 1.x, "letter" is a no-op, so $later_letter
	    # will ~always be true and should be ignored.  Increase
	    # chunk until we see repeats.

	    # In Mailman 2.x, we need to iterate through both letter
	    # and chunk, but if the list has few members, they will
	    # all be listed and letter and chunk are ignored.  Also,
	    # if there are no members for a given letter, the whole
	    # list will be returned.

	    if ($repeated) {
		last member_letter if $later_letter;
		next member_letter;
	    }

	    # The maximum number of addresses on each page can be
	    # configured, by default it is set to 30, but it could in
	    # theory be less.  To save time, we assume that we have
	    # all the members if we got less than 20 addresses.
	    next member_letter if $count < 20;

	    ++$chunk; $params{chunk} = $chunk;
	    $resp = $ua->post("$url?letter=$letter&chunk=$chunk", \%params);
	}
    }
    if ($config->{meta_members}) {
	push(@addresses, split(/\n+/, $config->{meta_members}));
    }
    return @addresses;
}

# This code is only useful on the patched Mailman 1.2 installation at
# UiO.  Notice that it uses GET without any parameters to fetch the
# page, since otherwise it will clear the meta members.
# Unfortunately, this means we need to use cookies to log in, and this
# requires a new Perl module, WWW::Mechanize.  Since this is such a
# site specific feature, we hide the requirement so listadmin runs
# even without the module.

sub fetch_meta_members {
    my ($list, $config) = @_;

    return if defined $config->{meta_members}; # already fetched
    return unless $config->{meta_member_support} || $list =~ /\buio\.no$/i;

    # We will only attempt this once, so make a note we've tried.
    $config->{meta_members} = "";

    unless (eval "require WWW::Mechanize; 1") {
	print "WARNING: Meta members may be removed, install WWW::Mechanize\n";
	return;
    }

    my $agent = WWW::Mechanize->new(autocheck => 1);
    $agent->get(mailman_url($list, $config->{adminurl}));
    $agent->submit_form(fields => { username => $config->{user},
				    adminpw => $config->{password}});

    $agent->get(mailman_url($list, $config->{adminurl}, "", "members"));

    my $page = $agent->content();
    my $parse = HTML::TokeParser->new(\$page);
    my $tag = $parse->get_tag("textarea");
    $tag = $parse->get_tag("textarea");
    return unless defined $tag; # silently ignore the failure

    if ($tag->[1]->{name} eq "meta_members") {
	$config->{meta_members} = $parse->get_trimmed_text("/textarea");
    }
}

sub remove_matching_subscribers {
    my ($list, $config, $pattern) = @_;
    my @addresses = list_subscribers($list, $config);
    if (defined($pattern) and $pattern ne "") {
	@addresses = grep { /$pattern/ } @addresses;
    }
    my $msg = remove_subscribers($list, $config, @addresses);
    if ($msg eq "OK") {
	print "Removed:\n  ", join("\n  ", @addresses), "\n";
    } else {
	print $msg, "\n";
    }
}

sub read_address_file {
    my ($file, $assert_nonempty) = @_;
    my @list = ();
    open(F, $file) || die "$file: $!\n";
    while (<F>) {
	s/(^|\s)\#.*//;
	s/^\s+//;
	s/\s+$//;
	next if /^$/;
	push(@list, $_);
    }

    die "$file: no lines, aborting\n" if $assert_nonempty && @list == 0;
    return @list;
}

sub submit_http {
    my ($url, $params, $log, $logfile) = @_;

    my $opened;
    if ($logfile) {
	if (open (LOG, ">>$logfile")) {
	    LOG->autoflush(1);
	    # Perhaps we should force the encoding to US-ASCII
	    # instead, but I think this is more DWIM compliant.
	    binmode LOG, ":encoding($term_encoding)";
	    $opened = 1;
	    local $SIG{__WARN__} = sub {}; # see comment elsewhere
	    print LOG $log;
	} else {
	    print STDERR "WARNING: Failed to append to $logfile: $!\n";
	}
    }
    my $ret = $ua->post ($url, $params);
    print STDERR "server returned error\n", $ret->error_as_HTML, "\n"
	    unless $ret->is_success;
    if ($opened) {
	if ($ret->is_success) {
	    print LOG "changes sent to server\n";
	} else {
	    print LOG "server returned error\n", $ret->error_as_HTML, "\n";
	}
	close (LOG);
    }
}

sub got_match {
    my ($str, $pattern) = @_;

    return undef unless defined ($str) && $pattern;

    # If the pattern is delimited by slashes, run it directly ...
    if ($pattern =~ m,^/(.*)/([ix]*)$,) {
	eval "\$str =~ $pattern";
    } else {
	$str =~ $pattern;
    }
}

sub restore_echo_and_exit {
    system("stty echo");
    print "\n";
    exit(1);
}

sub prompt_password {
    my ($prompt) = @_;
    my $answer;
    my $echooff;

    # This might not work, since some versions of readline screw up
    # and turn on "echo" for us :-(

    $SIG{'INT'} = $SIG{'TERM'} = \&restore_echo_and_exit;
    system("stty -echo 2>/dev/null");
    if ($? == 0) {
	$echooff = 1;
    } else {
	$prompt .= "(will appear on screen): ";
    }
    $answer = prompt($prompt);
    if ($echooff) {
	print "\n";
	system("stty echo");
	$SIG{'INT'} = $SIG{'TERM'} = 'DEFAULT';
    }
    return $answer;
}
    
sub prompt {
    # $term is a global variable.  we initialise it here, so that it
    # is only done if the user actually needs prompting.
    $term = new Term::ReadLine 'listadmin'
	    unless $term;
    my $answer = $term->readline(@_);
    # readline turns off autoflush, re-enable it
    $| = 1;
    return $answer;
}

sub config_order {
    $config->{$a}{order} <=> $config->{$b}{order};
}
