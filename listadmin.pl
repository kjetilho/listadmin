#! /local/bin/perl5
#
# listadmin version 2.13
# Written 2003, 2004 by
# Kjetil Torgrim Homme <kjetilho+listadmin@ifi.uio.no>
# Released into public domain.

use HTML::TokeParser;
use LWP::UserAgent;
use MIME::Base64;
use MIME::QuotedPrint;
use Data::Dumper;
use Term::ReadLine;
use IO::Handle;
use strict;

sub usage {
    print STDERR "Usage: $0 [-f CONFIGFILE] [listname]\n";
    exit (64);
}

my $term;
my $ua = new LWP::UserAgent ("timeout" => 600);
my $rc = $ENV{"HOME"}."/.listadmin.ini";
my $oldconf = $ENV{"HOME"}."/.listconf";
upgrade_config($oldconf, $rc);

if (@ARGV >= 2 && $ARGV[0] eq "-f") {
    shift; $rc = shift;
}
usage if (@ARGV > 1);

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

my ($from, $subject, $reason, $spamscore);

format STDOUT =
From:    @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         $from
Subject: ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         $subject
~~       ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         $subject
Reason:  @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Spam? @<<
         $reason,                                             $spamscore
.


for my $list (@lists) {
    my $user = $config->{$list}{"user"};
    my $pw = $config->{$list}{"password"};

    print "fetching data for $list\n";
    my $info = get_list ($list, $config->{$list}{"adminurl"}, $user, $pw);
    my %change = ();

    process_subscriptions ($list, $info, $config->{$list}, \%change);
    approve_messages ($list, $info, $config->{$list}, \%change);

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
	    unless ($c =~ /^\s*(|ja?|y|yes)\s*$/) {
		print "skipping ...\n";
		next;
	    }
	}
    }

    commit_changes ($list, $user, $pw, $config->{$list}{"adminurl"},
		    \%change, $info, $config->{$list}{"logfile"});
}

sub process_subscriptions {
    my ($list, $info, $config, $change) = @_;
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
	++$num;
	print "\n[$num/$count] ======= \#$id of $list =======\n";
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
	    last subscr_loop if $ans eq "q";
	    next subscr_loop if $ans eq "s";
	    if ($ans eq "a") {
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
    my ($list, $info, $config, $change) = @_;

    my $def = $config->{"default"};
    my $spamlevel = $config->{"spamlevel"};
    my $ns_from = $config->{"not_spam_if_from"};
    my $ns_subj = $config->{"not_spam_if_subject"};
    my $dis_from = $config->{"discard_if_from"};
    my $dis_subj = $config->{"discard_if_subject"};
    my $dis_reas = $config->{"discard_if_reason"};

    my $count = keys (%{$info}) - 1;	# subtract 1 for globals
    my $num = 0;
    my $prompt = 'Approve/Reject/Discard/Skip/view Body/view Full/Help/Quit';
    $prompt .= " [" . uc($def) . "]" if $def;
    $prompt .= " ? ";

 msgloop:
    for my $id (sort keys %{$info}) {
	next if $id eq "global";
	++$num;
	$from = $info->{$id}{"from"};
	$subject = $info->{$id}{"subject"};
	$reason = $info->{$id}{"reason"};
	$spamscore = $info->{$id}{"spamscore"};
	print "\n[$num/$count] ======= \#$id of $list =======\n";
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
			     ($ns_from && $from =~ $ns_from));

	    if ($ans && $match) {
		if ($match eq "spam") {
		    print "Automatically discarded as spam.\n";
		} else {
		    print "Automatically discarded due to matching $match\n";
		}
		$ans = "d";
	    }
	    
	    $ans ||= prompt ($prompt);
	    $ans = "q" unless defined $ans;
	    $ans =~ s/\s+//g;
	    $ans = $def if $ans eq "";
	    $ans = lc $ans;
	    last msgloop if $ans eq "q";
	    next msgloop if $ans eq "s";
	    if ($ans eq "a" || $ans eq "d") {
		$change->{$id} = [ $ans ];
		last;
	    } elsif ($ans eq "r") {
	    redo_reject:
		my $r = prompt ("Why do you reject? ",
					 $info->{$id}{"rejreason"});
		if ($r =~ /^\s*$/) {
		    print "aborted\n";
		    next;
		} elsif ($r =~ /^\s*(\?+|h|help)\s*$/i) {
		    print "The reason entered will be included in the e-mail ".
			    "sent to the submitter.\n";
		    goto redo_reject;
		}

		$change->{$id} = [ "r", $r ];
		last;
	    } elsif ($ans eq "f") {
		print $info->{$id}{"headers"}, "\n", $info->{$id}{"body"};
	    } elsif ($ans eq "b") {
		my $head = lc $info->{$id}{"headers"};
		my $text = $info->{$id}{"body"};
		if ($head =~ /content-type:\s+text\/\S+\s+charset="?(iso-8859-15?|us-ascii|utf-8)"?/) {
		    my $charset = $1;
		    if ($head =~ /content-transfer-encoding: (base64|quoted-printable)/) {
			if ($1 eq "base64") {
			    $text = MIME::Base64::decode_base64($text);
			} else {
			    $text = MIME::QuotedPrint::decode($text);
			}
		    }
		    $text = utf8_to_latin1 ($text) if $charset eq "utf-8";
		}
		my @lines = split (/\n/, $text, 21);
		pop @lines;
		print join ("\n", @lines), "\n";
	    } elsif ($ans eq "") {
		# nothing.
	    } else {
		print <<"end";
Choose one of the following actions by typing the corresponding letter
and pressing Return.

  a  Approve   -- the message will be sent to all member of the list
  r  Reject    -- notify sender that the message was rejected
  d  Discard   -- throw message away, don't notify sender
  s  Skip      -- don't decide now, leave it for later
  b  view Body -- display the first 20 lines of the message
  f  view Full -- display the complete message, including headers
  q  Quit      -- go on to the next list

end
		print <<"end" if $def;
The default action for this list when you only press Return is '$def'

end
            }
	}
    }
}


sub mailman_url {
    my ($list, $pattern, $user, $pw) = @_;

    $pw =~ s/(\W)/sprintf("%%%02x", ord($1))/ge;

    my $args = "username=$user&adminpw=$pw";
    my ($lp, $domain) = split ('@', $list);
    if ($pattern) {
	my $url = $pattern;
	my $subdom = $domain;
	$subdom = $` if $subdom =~ /\./;
	$url =~ s/\{list\}/$lp/g;
	$url =~ s/\{domain\}/$domain/g;
	$url =~ s/\{subdomain\}/$subdom/g;
	return "$url?$args";
    }

    my $www;
    if ($domain eq "lister.ping.uio.no") {
	return "https://$domain/mailman/$domain/admindb/$lp?$args";
    } elsif ($domain =~ /^(\w+)\.uio\.no$/) {
	$www = "$1-lists.uio.no";
    } elsif ($domain eq "uio.no") {
	$www = "uio-lists.uio.no";
    } else {
	$www = "lister.uio.no";
    }
    return "http://$www/mailman/admindb/$list?$args";
}

sub get_list {
    my ($list, $url, $user, $pw) = @_;

    # where we gather all the information about pending messages
    my %data = ();
    my $starttime = time;

    my $page;

    my $resp = $ua->get (mailman_url ($list, $url, $user, $pw));
    $page = $resp->content;

    # save it for eased debug for the developer...
    if ($< == 1232 && open (DUMP, ">/tmp/dump-$list.html")) {
	print DUMP $page;
	close (DUMP);
    }

    unless ($resp->is_success) {
	print STDERR $resp->error_as_HTML;
	return ();
    }
    my $parse = HTML::TokeParser->new(\$page) || die;

    $parse->get_tag ("title") || die;
    my $title = $parse->get_trimmed_text ("/title") || die;
    if ($title =~ /authentication/i) {
	print STDERR
		"Unable to log in. Is your username and password correct?\n";
	return ();
    }
    my $mmver;

    $parse->get_tag ("hr");
    $parse->get_tag ("h2") || return ();
    my $headline = $parse->get_trimmed_text ("/h2") || die;
    if ($headline =~ /subscription/i) {
	parse_subscriptions ($parse, \%data);
    } elsif ($headline =~ /held for approval/i) {
	$mmver = parse_approvals ($parse, \%data);
    } else {
	$parse->get_tag ("hr") || die;
	my $token = $parse->get_token;
	if ($token->[0] eq "S" && lc ($token->[1]) eq "center") {
	    $mmver = parse_approvals ($parse, \%data);
	}
    }
    return () unless parse_footer ($parse, \%data, $mmver);
    return (\%data);
}

sub parse_subscriptions {
    my ($parse, $data) = @_;
    my $token;

    $parse->get_tag ("table") || die;
    $parse->get_tag ("tr") || die;
    $parse->get_tag ("tr") || die;
    do {
	parse_subscription ($parse, $data);
	do {
	    $token = $parse->get_token;
	} until ($token->[0] eq "S");
    } while (lc ($token->[1]) eq "tr");
}

sub parse_subscription {
    my ($parse, $data) = @_;

    $parse->get_tag ("td") || die;
    my $address = $parse->get_trimmed_text ("/td") || die;
    my $tag = $parse->get_tag ("input") || die;
    my $id = $tag->[1]{"name"};
    $parse->get_tag ("/table") || die;
    $parse->get_tag ("/tr") || die;
    $data->{$id} = { "subscription" => $address };
}

sub parse_approvals {
    my ($parse, $data) = @_;
    my $token;
    my $mmver;

    do {
	$parse->get_tag ("table") || die;
	my $ret = parse_approval ($parse, $data);
	$mmver = $ret if $ret;
	$parse->get_tag ("/table");
	$parse->get_tag ("hr");
	$token = $parse->get_token;
    } until ($token->[0] eq "S" && lc ($token->[1]) eq "input");
    return ($mmver);
}

# NB! lossy!
sub utf8_to_latin1 {
    my $self = shift;
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

sub parse_approval {
    my ($parse, $data) = @_;
    my ($from, $reason, $subject, $id, $mmver, $body, $headers);

    $parse->get_tag ("tr") || die;
    $parse->get_tag ("td") || die;
    $parse->get_tag ("td") || die;
    $from = $parse->get_trimmed_text("/td");

    $parse->get_tag ("tr") || die; # Reason: _or_ Subject:
    $parse->get_tag ("td") || die;
    my $field = $parse->get_trimmed_text ("/td");
    $parse->get_tag ("td") || die; 
    if ($field =~ /Reason/) {
	$mmver = 1.2;
	$reason = $parse->get_trimmed_text("/td");
	$parse->get_tag ("tr") || die; # Subject:
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die;
	$subject = $parse->get_trimmed_text("/td");
    } else {
	$mmver = 2;
	$subject = $parse->get_trimmed_text("/td");
	$parse->get_tag ("tr") || die; # Reason:
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die;
	$reason = $parse->get_trimmed_text("/td");
    }
    my $utf8 = 0;

    # this will also decode invalid tokens, where the encoded word is
    # concatenated with other letters, e.g.  foo=?utf-8?q?=A0=F8?=
    $subject =~ s/=\?(us-ascii|utf-8|iso-8859-15?)\?q\?(.*?)\?=/
	MIME::QuotedPrint::decode($2)/ieg;
    $utf8 ||= 1 if $1 =~ /utf-8/i;
    $subject =~ s/=\?(us-ascii|utf-8|iso-8859-15?)\?b\?(.*?)\?=/
	MIME::Base64::decode_base64($2)/ieg;
    $utf8 ||= 1 if $1 =~ /utf-8/i;
    $subject = utf8_to_latin1 ($subject) if $utf8;

    $parse->get_tag ("tr") || die; # Action:
    my $tag = $parse->get_tag ("input") || die;
    $id = $tag->[1]{"name"};

    $data->{$id} = { "from" => $from,
		     "subject" => $subject,
		     "reason" => $reason };

    $parse->get_tag ("tr") || die; # Reject _or_ Preserve message
    if ($mmver >= 2) {
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
    $data->{$id}->{"spamscore"} = 0;
    $data->{$id}->{"spamscore"} = length ($1)
	    if $headers =~ /^X-UiO-Spam-score: (s+)/m;
    $data->{$id}->{"date"} = "<no date>";
    $data->{$id}->{"date"} = $1
	    if $headers =~ /^Date: (.*)$/m;

    if ($mmver == 2) {
	$parse->get_tag ("tr") || die;  # Message Excerpt
	$parse->get_tag ("td") || die;
	$parse->get_tag ("textarea") || die;
	$body = $parse->get_text("/textarea");
    } else {
	$headers =~ s/\n\n//s;
	$body = $';
	$headers = $`;
    }
    $body .= "\n" unless $body =~ /\n$/;
    $data->{$id}->{"headers"} = $headers;
    $data->{$id}->{"body"} = $body;

    return ($mmver);
}

sub parse_footer {
    my ($parse, $data, $mmver) = @_;

    $parse->get_tag ("address") || die;
    my $text = $parse->get_trimmed_text ("/address") || die;

    if ($text =~ /Mailman\s*v(ersion)? (\d+\.\d+)/) {
	if ($mmver && $mmver != 0 + $2) {
	    print STDERR "Unknown version of Mailman.  First I thought ",
	        "this was version $mmver.\n", "Now version ", 0 + $2,
	        " looks more likely.  Help!\n";
	    return (0);
	}
	$mmver = 0 + $2;
    }

    if ($mmver == 2) {
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
    return (1);
}

# .listconf was the configuration file for the previous listadmin
# script, which was written in Bash and simply sourced the file...
sub upgrade_config {
    my ($conf, $rc) = @_;
    return if -f $rc;
    return unless -f $conf;

    print "Converting to new configuration file, $rc\n\n";

    my $cmd = ". $conf; umask 077; (". <<'END' . ") > $rc";
      printf "# automatically converted from .listconf\r\n";
      printf "#\r\n";
      printf "username $LISTUSER\r\n";
      printf "password \"$LISTPASS\"\r\n";
      printf "spamlevel 12\r\n";
      printf "not_spam_if_from uio\.no\n";
      printf "default discard\r\n";
      printf "# uncomment the following to get a terse transaction log\r\n";
      printf "# log \"~/.listadmin.log\"\r\n";
      printf "\r\n";
      for l in $LISTS; do printf "$l\r\n"; done
END
    system $cmd;
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
    my $confirm = 1;
    my $url;
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
	next if /^\s*#/;
	s/^\s+// if $line;	# remove leading whitespace after continuation
	if (/\\$/) {
	    $line .= $`; # $PREFIX
	    next;
	}
	$line .= $_;
	$line =~ s/^\s+//;
	next if /^$/;
	if ($line =~ /^username\s+/i) {
	    $user = unquote ($'); # $POSTFIX
	    if ($user !~ /^[a-z0-9._-]+\@[a-z0-9.-]+$/) {
		print STDERR "$file:$lineno: Illegal username: '$user'\n";
		exit 1;
	    }
	} elsif ($line =~ /^password\s+/i) {
	    $pw = unquote ($');
	} elsif ($line =~ /^spamlevel\s+/i) {
	    $spam = unquote ($');
	    if ($spam =~ /^(\d+)\s*$/) {
		$spam = $1;
	    } else {
		print STDERR "$file:$lineno: Illegal value: '$spam'\n";
		print STDERR "choose a positive numeric value\n";
		exit 1;
	    }
	} elsif ($line =~ /^confirm\s+/i) {
	    $confirm = unquote ($');
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
	    $action = unquote ($'); # $POSTFIX
	    unless (exists $act{$action}) {
		print STDERR "$file:$lineno: Illegal value: '$action'\n";
		print STDERR "choose one of ",
                             join (", ", sort keys %act), "\n";
		exit 1;
	    }
	    $action = $act{$action};
	} elsif ($line =~ /^adminurl\s+/i) {
	    $url = unquote ($'); # $POSTFIX
	    $url = undef if $url eq "NONE";  # use UiO specific code
	} elsif ($line =~ /^default\s+/i) {
	    $default = unquote ($'); # $POSTFIX
	    unless (exists $act{$default}) {
		print STDERR "$file:$lineno: Illegal value: '$default'\n";
		print STDERR "choose one of ",
                             join (", ", sort keys %act), "\n";
		exit 1;
	    }
	    $default = $act{$default};
	} elsif ($line =~ /^log\s+/i) {
	    $logfile = unquote ($'); # $POSTFIX
	    $logfile =~ s,^\$HOME/,$ENV{'HOME'}/,;
	    $logfile =~ s,^~/,$ENV{'HOME'}/,;
	    $logfile =~ s,^~(\w+)/,(getpwnam($1))[7]."/",e;
	    if ($logfile =~ /^M:/i) {
		$logfile =~ s,\\,/,g;
		$logfile =~ s,^M:,$ENV{'HOME'},;
	    }
	    $logfile = undef if $logfile eq "none";
	} elsif ($line =~ /^subscription_action\s+/) {
	    $subact = unquote ($');
	    unless (exists $sact{$subact}) {
		print STDERR "$file:$lineno: Illegal value: '$subact'\n";
		print STDERR "choose one of ",
		             join (", ", sort keys %sact), "\n";
		exit 1;
	    }
	    $subact = $sact{$subact};
	} elsif ($line =~ /^subscription_default\s+/) {
	    $subdef = unquote ($');
	    unless (exists $sact{$subdef}) {
		print STDERR "$file:$lineno: Illegal value: '$subdef'\n";
		print STDERR "choose one of ",
		             join (", ", sort keys %sact), "\n";
		exit 1;
	    }
	    $subdef = $sact{$subdef};
	} elsif ($line =~ /^($pattern_keywords)\s+/o) {
	    my $key = $1;
	    my $val = $'; # $POSTFIX
	    $val =~ s/\s+$//;
	    if ($val =~ /^"(.*)"$/) {
		$val = $1;
		$val =~ s/\\"/"/g;
		$val =~ s/\\\\/\\/g;
	    }
	    $patterns{$key} = ($val eq "NONE") ? undef : $val;
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
    my $pass = prompt ("Enter Mailman password (will appear on screen): ");
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
	$list =~ s/\s*//g;
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

    my $baseurl = mailman_url ($list, $url, $user, $pw);
    my $action = $msgs->{"global"}{"actions"};
    my $changes = 0;

    my $log = log_timestamp ($list);
    my $url = $baseurl;

    for my $id (keys %{$change}) {
	my ($what, $text) = @{$change->{$id}};
	$url .= "&$id=" . $action->{$what};
	$log .= sprintf ("%s D:[%s] F:[%s] S:[%s]\n",
			 $what,
			 $msgs->{$id}{"date"},
			 $msgs->{$id}{"from"},
			 $msgs->{$id}{"subject"});
	if ($what == "r") {
	    $text =~ s/(\W)/sprintf("%%%02x", ord($1))/ge;
	    $url .= "&comment-$id=$text";
	}
	++$changes;

	# HTTP does not specify a maximum length for the URI in a GET
	# request, but it recommends that a server does not rely on
	# clients being able to send URIs larger than 255 octets.  the
	# reject reason can be very long, so theoretically, we can
	# overshoot that limit even if we change the 1000 below into
	# 250.  Mailman has been observed to reject URI's ~3400 octets
	# long, but accept 8021.  the limit is probably based on the
	# time taken to process, rather than the length of the URI.
	# in times with high load on the Mailman server, it's best to
	# keep the amount of work per request down.

	if (length ($url) > 500) {
	    submit_http ($url, $log, $logfile);
	    $url = $baseurl;
	    $log = log_timestamp ($list);
	    $changes = 0;
	}
    }
    submit_http ($url, $log, $logfile)
	    if $changes;
}

sub log_timestamp {
    my $list = shift;

    my ($sec, $min, $hour, $mday, $mon, $year) = (localtime (time))[0..5];
    return (sprintf ("submitting %s %04d-%02d-%02dT%02d:%02d:%02d\n",
		     $list, $year+1900, $mon+1, $mday, $hour, $min, $sec));
}

sub submit_http {
    my ($url, $log, $logfile) = @_;

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
    my $ret = $ua->get ($url);
    print STDERR "server returned error\n", $ret->error_as_HTML, "\n"
	    unless $ret->is_success;
    if ($opened) {
	if ($ret->is_success) {
	    print LOG "changes sent to server ";
	} else {
	    print LOG "server returned error\n", $ret->error_as_HTML, "\n";
	}
	print LOG "(URI length ", length ($url), ")\n";
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
