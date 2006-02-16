#! /usr/bin/perl -w
#
# listadmin version 2.28
# Written 2003 - 2006 by
# Kjetil Torgrim Homme <kjetilho+listadmin@ifi.uio.no>
#
# Thank you, Sam Watkins and Bernie Hoeneisen, for contributions and
# feedback.
#
# Released into public domain.

use HTML::TokeParser;
use LWP::UserAgent;
use MIME::Base64;
use MIME::QuotedPrint;
use Data::Dumper;
use Term::ReadLine;
use Getopt::Std;
use strict;
use English;

my $rc = $ENV{"HOME"}."/.listadmin.ini";

sub usage {
    print STDERR <<_end_;
Usage: $0 [-f CONFIGFILE] [-t MINUTES] [LISTNAME]
  -f CONFIGFILE    Read configuration from CONFIGFILE.
                   (default: $rc)
  -t MINUTES       Stop processing after MINUTES minutes.  Decimals are
                   allowed.
  LISTNAME         Only process lists with name matching LISTNAME.
_end_
    exit (64);
}

my $term;
my $ua = new LWP::UserAgent ("timeout" => 600);

our ($opt_f, $opt_t);

usage() unless getopts('f:t:');
$rc = $opt_f if $opt_f;
usage() if defined $opt_t && $opt_t !~ /\d/ && $opt_t !~ /^\d*(\.\d*)?$/;
my $time_limit = time + 60 * ($opt_t || 24*60);
my $hostname = `/bin/uname -n`;
chomp($hostname);
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

my ($num, $count, $list, $from, $subject, $reason, $spamscore);

format STDOUT =

@<<<<<<< ========== @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
sprintf("[%d/%d]", $num, $count), $list." "."=" x (51 - length($list))
From:    @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         $from
Subject: ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         $subject
~~       ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         $subject
Reason:  @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Spam? @<<
         $reason,                                             $spamscore
.


for $list (@lists) {
    my $user = $config->{$list}{"user"};
    my $pw = $config->{$list}{"password"};

    if (time > $time_limit) {
	print "Time's up, skipping the remaining lists\n";
	last;
    }

    my $info = {};
    print "fetching data for $list\n";
    do {
	if ($pw eq "" || $info->{'autherror'}) {
	    $pw = prompt_password("Enter password" .
				  ($user ? " for $user: ": ": "));
	}
	$info = get_list ($list, $config->{$list}, $pw) if $pw;
    } while ($info->{'autherror'} && $pw);
    if ($info->{'servererror'} || $info->{'autherror'}) {
	print "skipping...\n";
	next;
    }

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
unless you answer this question affirmatively.
_END_
		goto redo_confirm;
	    }
	    if ($c =~ /^\s*(no?|nei|skip)\s*$/i) {
		print "skipping ...\n";
		next;
	    } elsif ($c =~ /^\d$/) {
		$num = $c - 1;
		goto restart_approval;
	    } elsif ($c !~ /^\s*(|ja?|y|yes)\s*$/i) {
		goto redo_confirm;
	    }
	}
    }

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
    my $def = $config->{"subdef"};
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
	    $ans = $config->{"subact"};
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
		print STDERR <<end;
Choose one of the following actions by typing the corresponding letter
and pressing Return.

  a  Accept    -- allow the user to join the mailing list
  r  Reject    -- notify sender that the request was turned down
  s  Skip      -- do not decide now, leave it for later
  q  Quit      -- go on to approving messages

end
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
    my $prompt = 'Approve/Reject/Discard/Skip/view Body/view Full/jump #/Help/Quit';
    my @num_to_id = grep { ! /^global$/ } sort keys %{$info};
 msgloop:
    while ($num < $count) {
	last if time > $time_limit;
	my $id = $num_to_id[$num++];
	$from = $info->{$id}{"from"};
	$subject = $info->{$id}{"subject"} || "";
	$reason = $info->{$id}{"reason"};
	$spamscore = $info->{$id}{"spamscore"};
	write;

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
	    $ans =~ s/\s+//g;
	    $ans = $def if $ans eq "" && defined $def;
	    $ans = lc $ans;
	    if ($ans eq "q") {
		last msgloop;
	    } elsif ($ans eq "s") {
		delete $change->{$id};
		$dont_skip_forward = 0;
		next msgloop;
	    } elsif ($ans =~ /^\d+$/ && $ans > 0 && $ans <= $count) {
		$num = $ans - 1;
		$dont_skip_forward = 1;
		next msgloop;
	    } elsif ($ans eq "a" || $ans eq "d") {
		$change->{$id} = [ $ans ];
		$dont_skip_forward = 0;
		last;
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

		$change->{$id} = [ "r", $r ];
		$dont_skip_forward = 0;
		last;
	    } elsif ($ans eq "f") {
		print $info->{$id}{"headers"}, "\n\n", $info->{$id}{"body"};
	    } elsif ($ans eq "b") {
		my $head = lc $info->{$id}{"headers"};
		my $text = $info->{$id}{"body"};
		if ($head =~ m,content-type:\s+text/,) {
		    my $charset = "UNKNOWN";
		    if ($head =~ /charset="?(iso-8859-15?|us-ascii|utf-8)"?/) {
			$charset = $1;
		    }
		    if ($head =~ /content-transfer-encoding:\s+quoted-print/) {
			$text = MIME::QuotedPrint::decode($text);
		    } elsif ($head =~ /content-transfer-encoding:\s+base64/) {
			$text = MIME::Base64::decode_base64($text);
		    }
		    $text = utf8_to_latin1 ($text) if $charset eq "utf-8";
		}
		my @lines = split (/\n/, $text, 21);
		pop @lines;
		print join ("\n", @lines), "\n";
	    } elsif ($ans eq "t") {
		print $info->{$id}{"date"}, "\n";
	    } elsif ($ans eq "url") {
		print mailman_url($list, $config->{adminurl}), "\n";
	    } elsif ($ans eq ".") {
		# write modifies $subject, so reinitialise it
		$subject = $info->{$id}{"subject"} || "";
		write;
	    } elsif ($ans eq "") {
		# nothing.
	    } else {
		print <<"end";
Choose one of the following actions by typing the corresponding letter
and pressing Return.

  a  Approve   -- the message will be sent to all members of the list
  r  Reject    -- notify sender that the message was rejected
  d  Discard   -- throw message away, don't notify sender
  s  Skip      -- don't decide now, leave it for later
  b  view Body -- display the first 20 lines of the message
  f  view Full -- display the complete message, including headers
  t  view Time -- display the date the message was sent
  #  jump      -- jump backward or forward to message number #
  /pattern     -- search for next message with matching From or Subject
  ?pattern     -- search for previous message with matching From or Subject
  .            -- redisplay entry
  q  Quit      -- go on to the next list

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
    my ($list, $pattern, $params) = @_;

    my ($lp, $domain) = split ('@', $list);

    $pattern ||= uio_adminurl ($domain);
    $pattern ||= 'http://{domain}/mailman/admindb/{list}';

    my $url = $pattern;
    my $subdom = $domain;
    $subdom = $PREMATCH if $subdom =~ /\./;
    $url =~ s/\{list\}/$lp/g;
    $url =~ s/\{domain\}/$domain/g;
    $url =~ s/\{subdomain\}/$subdom/g;
    $url .= "?$params" if $params;
    return $url;
}

# Returns a ref to a hash with all the information about pending messages
sub get_list {
    my ($list, $config, $pw) = @_;

    my $starttime = time;
    my $mmver;
    my ($page, $page_appr, $resp_appr);
    my $resp = $ua->post(mailman_url($list, $config->{"adminurl"}),
			 mailman_params($config->{"user"}, $pw));
    unless ($resp->is_success) {
	print STDERR $resp->error_as_HTML;
	return {'servererror' => 1};
    }
    $page = $resp->content;

    my $parse = HTML::TokeParser->new(\$page) || die;
    $parse->get_tag ("title") || die;
    my $title = $parse->get_trimmed_text ("/title") || die;
    if ($title =~ /authentication/i) {
	print STDERR
		"Unable to log in. Is your username and password correct?\n";
	return {'autherror' => 1};
    }

    my @mailman_mentions = grep {/Mailman/} split (/\n/, $page);
    my $last_mention = pop(@mailman_mentions);
    die "Can not find version information in '$last_mention'\n"
	    unless $last_mention =~ /\bv(ersion)?\s(\d+\.\d+)/;
    $mmver = $2;

    if ($mmver ge "2.1") {
	# Mailman does not look for "details" in parameters, so it
	# must be part of the query string.
	$resp = $ua->post(mailman_url($list, $config->{"adminurl"},
				      "details=all"),
			  mailman_params($config->{"user"}, $pw));
	unless ($resp->is_success) {
	    print STDERR $resp->error_as_HTML;
	    return {'servererror' => 1};
	}
	$page_appr = $resp->content;
    }

    my $dumpdir = $config->{$list}{"dumpdir"};
    if (defined $dumpdir) {
	if (open (DUMP, ">$dumpdir/dump-subs-$list.html")) {
	    print DUMP $page;
	    close (DUMP);
	}
	if ($page_appr && open (DUMP, ">$dumpdir/dump-held-$list.html")) {
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
    set_param_values($mmver, $data);
    return $data;
}

sub parse_pages_mm_old {
    my ($mmver, $config, $parse) = @_;

    my %data = ();
    my $token;
    $parse->get_tag ("hr");
    $parse->get_tag ("h2") || return ();
    my $headline = $parse->get_trimmed_text ("/h2") || die;
    if ($headline =~ /subscription/i) {
	parse_subscriptions ($mmver, $config, $parse, \%data);
	$token = $parse->get_token;
	if (lc ($token->[1]) eq "input") {
	    return (\%data);
	} else {
	    $parse->get_tag ("h2") || die;
	    $headline = $parse->get_trimmed_text ("/h2") || die;
	}
    }
    if ($headline =~ /held for approval/i) {
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

# NB! lossy!
sub utf8_to_latin1 {
    my ($s) = @_;
    $s =~ s/([\x80-\xff][\x80-\xbf]*)/&utf8_to_latin1_char($1)/ge;
    return $s;
}

sub utf8_to_latin1_char {
    my($first, @rest) = unpack('C*', $_[0]);
    $first ^= 0xC2;
    return chr($first * 0x40 + $rest[0]) if $first < 2 && @rest == 1;
    # We simply remove the other codes, they obviously won't fit in Latin1.
    return "";
}

sub decode_rfc2047_qp {
    my $text = shift;
    $text =~ s/_/ /g;
    return MIME::QuotedPrint::decode ($text);
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
    my $utf8 = 0;
    # this will also decode invalid tokens, where the encoded word is
    # concatenated with other letters, e.g.  foo=?utf-8?q?=A0=F8?=
    $subject =~ s/=\?(us-ascii|utf-8|iso-8859-15?)\?q\?(.*?)\?=/
	    decode_rfc2047_qp($2)/ieg;
    $utf8 ||= 1 if defined $1 && $1 =~ /utf-8/i;
    $subject =~ s/=\?(us-ascii|utf-8|iso-8859-15?)\?b\?(.*?)\?=/
	MIME::Base64::decode_base64($2)/ieg;
    $utf8 ||= 1 if defined $1 && $1 =~ /utf-8/i;
    $subject = utf8_to_latin1 ($subject) if $utf8;

    $parse->get_tag ("tr") || die;	# Action:
    my $tag = $parse->get_tag ("input") || die;
    $id = $tag->[1]{"name"};

    $data->{$id} = { "from" => $from,
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

    # Extract the length from all spam score headers, sort them in
    # descending order, and pick the front (max) element:
    my ($score) = sort {$b <=> $a}
                  map {length} 
                  $headers =~ /^$header_re:\s+
			       (?:\d+\.\d+\s+)? \(?(\S+)\)?/xgim;

    $data->{$id}->{"spamscore"} = $score || 0;
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
    $headers =~ s/^\s+//;
    $body .= "\n" unless $body =~ /\n$/;
    $data->{$id}->{"headers"} = $headers;
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

    my ($user, $pw, $spam, $list);
    my $conf = {};
    my $line = "";
    my $subact;
    my $subdef;
    my $action = "";
    my $default = "";
    my $count = 0;
    my $lineno = 0;
    my $logfile;
    my $dumpdir;
    my $confirm = 1;
    my $url;
    my $spamheader;
    my %patterns = map { $_ => undef; }
                       qw (not_spam_if_from
			   not_spam_if_subject
			   discard_if_from
			   discard_if_subject
			   discard_if_reason);
    my $pattern_keywords = join ("|", keys %patterns);
    
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
	    $user = unquote($POSTMATCH);
	    if ($user !~ /^[a-z0-9._+-]+\@[a-z0-9.-]+$/) {
		print STDERR "$file:$lineno: Illegal username: '$user'\n";
		exit 1;
	    }
	} elsif ($line =~ /^password\s+/i) {
	    $pw = unquote($POSTMATCH);
	} elsif ($line =~ /^spamlevel\s+/i) {
	    $spam = unquote($POSTMATCH);
	    if ($spam =~ /^(\d+)\s*$/) {
		$spam = $1;
	    } else {
		print STDERR "$file:$lineno: Illegal value: '$spam'\n";
		print STDERR "choose a positive numeric value\n";
		exit 1;
	    }
	} elsif ($line =~ /^confirm\s+/i) {
	    $confirm = unquote($POSTMATCH);
	    if ($confirm eq "yes") {
		$confirm = 1;
	    } elsif ($confirm eq "no") {
		$confirm = undef;
	    } else {
		print STDERR "$file:$lineno: Illegal value: '$confirm'\n";
		print STDERR "choose one of yes or no\n";
		exit 1;
	    }
	} elsif ($line =~ /^action\s+/i) {
	    $action = unquote($POSTMATCH);
	    unless (exists $act{$action}) {
		print STDERR "$file:$lineno: Illegal value: '$action'\n";
		print STDERR "choose one of ",
                             join (", ", sort keys %act), "\n";
		exit 1;
	    }
	    $action = $act{$action};
	} elsif ($line =~ /^adminurl\s+/i) {
	    $url = unquote($POSTMATCH);
	    $url = undef if $url eq "NONE";
	} elsif ($line =~ /^default\s+/i) {
	    $default = unquote($POSTMATCH);
	    unless (exists $act{$default}) {
		print STDERR "$file:$lineno: Illegal value: '$default'\n";
		print STDERR "choose one of ",
                             join (", ", sort keys %act), "\n";
		exit 1;
	    }
	    $default = $act{$default};
	} elsif ($line =~ /^log\s+/i) {
	    $logfile = expand_pathname(unquote($POSTMATCH));
	} elsif ($line =~ /^dumpdir\s+/i) {
	    $dumpdir = expand_pathname(unquote($POSTMATCH));
	    mkdir($dumpdir) if (defined $dumpdir);
	} elsif ($line =~ /^subscription_action\s+/) {
	    $subact = unquote($POSTMATCH);
	    unless (exists $sact{$subact}) {
		print STDERR "$file:$lineno: Illegal value: '$subact'\n";
		print STDERR "choose one of ",
		             join (", ", sort keys %sact), "\n";
		exit 1;
	    }
	    $subact = $sact{$subact};
	} elsif ($line =~ /^subscription_default\s+/) {
	    $subdef = unquote($POSTMATCH);
	    unless (exists $sact{$subdef}) {
		print STDERR "$file:$lineno: Illegal value: '$subdef'\n";
		print STDERR "choose one of ",
		             join (", ", sort keys %sact), "\n";
		exit 1;
	    }
	    $subdef = $sact{$subdef};
	} elsif ($line =~ /^($pattern_keywords)\s+/o) {
	    my $key = $1;
	    my $val = $POSTMATCH;
	    $val =~ s/\s+$//;
	    if ($val =~ /^"(.*)"$/) {
		$val = $1;
		$val =~ s/\\"/"/g;
		$val =~ s/\\\\/\\/g;
	    }
	    $patterns{$key} = ($val eq "NONE") ? undef : $val;
	} elsif ($line =~ /^spamheader\s+/) {
	    $spamheader = unquote($POSTMATCH);
	    unless ($spamheader =~ /^[\w-]+$/) {
		print STDERR "$file:$lineno: Illegal header name: ".
			"'$spamheader'\n";
		exit 1;
	    }
	    $spamheader = undef if $spamheader eq "default";
	} elsif ($line =~ /^([^@ \t]+@[^@])+\s*/) {
	    $conf->{$line} = { "user" => $user,
			       "password" => $pw,
			       "adminurl" => $url,
			       "spamlevel" => $spam,
			       "confirm" => $confirm,
			       "subact" => $subact,
			       "subdef" => $subdef,
			       "action" => $action,
			       "default" => $default,
			       "logfile" => $logfile,
			       "dumpdir" => $dumpdir,
			       "spamheader" => $spamheader,
			       %patterns,
			       "order" => ++$count,
			   };
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

	# HTTP does not specify a maximum length for the URI in a GET
	# request, but it recommends that a server does not rely on
	# clients being able to send URIs larger than 255 octets.  the
	# reject reason can be very long, so theoretically, we can
	# overshoot that limit even if we change the 500 below into
	# 250.  Mailman has been observed to reject URI's ~3400 octets
	# long, but accept 8021.  the limit is probably based on the
	# time taken to process, rather than the length of the URI.
	# in times with high load on the Mailman server, it's best to
	# keep the amount of work per request down.

	if ($changes > 50) {
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

sub submit_http {
    my ($url, $params, $log, $logfile) = @_;

    my $opened;
    if ($logfile) {
	if (open (LOG, ">>$logfile")) {
	    LOG->autoflush(1);
	    $opened = 1;
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
    return ($term->readline (@_));
}

sub config_order {
    $config->{$a}{order} <=> $config->{$b}{order};
}
