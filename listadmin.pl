#! /local/bin/perl5
#
# listadmin version 2.05
# Written 2003 by Kjetil Torgrim Homme <kjetilho@ifi.uio.no>
# Released into public domain.

use HTML::TokeParser;
use LWP::Simple;
use Data::Dumper;
use Term::ReadLine;
use IO::Handle;
use strict;

my $term = new Term::ReadLine 'listadmin';
my $rc = $ENV{"HOME"}."/.listadmin.ini";
my $oldconf = $ENV{"HOME"}."/.listconf";
upgrade_config($oldconf, $rc);

if (@ARGV >= 2 && $ARGV[0] eq "-f") {
    shift; $rc = shift;
}
if (@ARGV != 0) {
    print STDERR "Usage: $0 [-f CONFIGFILE]\n";
    exit (64);
}

my $config = read_config ($rc);

unless ($config) {
    exit (0) unless prompt_for_config ($rc);
    $config = read_config ($rc);
}

my ($info, $id, $subject);

format STDOUT =
From:    @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         $info->{$id}{"from"}
Subject: ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         $subject
~~       ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         $subject
Reason:  @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  Spam? @<<
         $info->{$id}{"reason"},               $info->{$id}{"spamscore"}
.

my $prompt =
'Approve/Reject/Discard/Skip/view Body/view Full/Help/Quit/eXit';

for my $list (sort {$config->{$a}{"order"} <=> $config->{$b}{"order"}}
	           keys %{$config}) {
    my $def = $config->{$list}{"default"};
    my $spamlevel = $config->{$list}{"spamlevel"};
    my $user = $config->{$list}{"user"};
    my $pw = $config->{$list}{"password"};

    print "fetching data for $list\n";
    $info = get_list ($list, $user, $pw);
    my $num = 0;
    my $count = keys (%{$info}) - 1;
    my %change = ();
    my $listprompt = $prompt;
    $listprompt = $prompt . " [" . uc($def) . "]" if $def;
    $listprompt .= " ? ";

 msgloop:
    for $id (sort keys %{$info}) {
	next if $id eq "global";
	++$num;
	$subject = $info->{$id}{"subject"};
	print "\n[$num/$count] ======= #$id of $list =======\n";
	write;

	while (1) {
	    my $ans;
	    if ($spamlevel && $info->{$id}{"spamscore"} >= $spamlevel) {
		print "Automatically discarded as spam.\n";
		$ans = "d";
	    }
	    $ans ||= $config->{$list}{"action"};
	    $ans ||= $term->readline ($listprompt);
	    $ans = "q" unless defined $ans;
	    $ans = $def if $ans eq "";
	    $ans =~ s/\s+//g;
	    $ans = lc $ans;
	    last msgloop if $ans eq "q";
	    next msgloop if $ans eq "s";
	    if ($ans eq "a" || $ans eq "d") {
		$change{$id} = [ $ans ];
		last;
	    } elsif ($ans eq "r") {
	    redo_reject:
		my $r = $term->readline ("Why do you reject? ",
					 $info->{$id}{"rejreason"});
		if ($r =~ /^\s*$/) {
		    print "aborted\n";
		    next;
		} elsif ($r =~ /^\s*(\?+|h|help)\s*$/i) {
		    print "The reason entered will be included in the e-mail ".
			    "sent to the submitter.\n";
		    goto redo_reject;
		}

		$change{$id} = [ "r", $r ];
		last;
	    } elsif ($ans eq "x") {
		%change = ();
		last msgloop;
	    } elsif ($ans eq "f") {
		print $info->{$id}{"excerpt"};
	    } elsif ($ans eq "b") {
		my $text = $info->{$id}{"excerpt"};
		$text =~ s/.*?\n\n//s;
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
  x  eXit      -- go on to the next list, undo all actions chosen

end
		print <<"end" if $def;
The default action for this list when you only press Return is '$def'

end
            }
	}
    }

    if ($config->{$list}{"confirm"}) {
	if (scalar %change) {
	redo_confirm:
	    my $c = $term->readline ("Submit changes? [yes] ");
	    if ($c =~ /^\s*(\?+|h|hj?elp)\s*$/i) {
		print "Nothing will be done to the messages in the administrative queue\nunless you answer this question affirmatively.\n";
		goto redo_confirm;
	    }
	    unless ($c =~ /^\s*(|ja?|y|yes)\s*$/) {
		print "skipping ...\n";
		next;
	    }
	}
    }

    commit_changes ($list, $user, $pw, \%change, $info,
		    $config->{$list}{"logfile"});
}

sub mailman_url {
    my ($list, $user, $pw) = @_;

    $pw =~ s/(\W)/sprintf("%%%02x", ord($1))/ge;

    my $args = "username=$user&adminpw=$pw";
    my ($lp, $domain) = split ('@', $list);
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
    my ($list, $user, $pw) = @_;

    # where we gather all the information about pending messages
    my %data = ();

    my $page = get (mailman_url ($list, $user, $pw));

    my $parse = HTML::TokeParser->new(\$page) || die;
    my ($from, $subject, $reason, $id, $tag, $excerpt, $spamscore, $rej);
    my $date;
    my $mailmanversion = 1;
    while ($parse->get_tag ("table")) {
    
	$parse->get_tag ("tr") || die; # From:_or_ at end
	$parse->get_tag ("td") || die;
	my $ver = $parse->get_trimmed_text ("/td") || die;
	last if $ver =~ /version/;
	$parse->get_tag ("td") || die;
	$from = $parse->get_trimmed_text("/td");

	$parse->get_tag ("tr") || die $page; # Reason:
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die;
	$reason = $parse->get_trimmed_text("/td");

	$parse->get_tag ("tr") || die; # Subject:
	$parse->get_tag ("td") || die;

	# the parsing just happens to fail here when the wrong
	# password is given...
	$parse->get_tag ("td") ||
		die "Parse failed.  Is your username and password correct?\n";
	$subject = $parse->get_trimmed_text("/td");

	$parse->get_tag ("tr") || die; # Action:
	$tag = $parse->get_tag ("input") || die;
	$id = $tag->[1]{"name"};

	$parse->get_tag ("tr") || die; # Reject _or_ Preserve message
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die;
	$rej = $parse->get_trimmed_text("/td") || die;
	if ($rej =~ /Preserve message/) {
	    $mailmanversion = 2;
	    $parse->get_tag ("tr") || die;    # forward
	    $parse->get_tag ("tr") || die;    # Reject
	    $parse->get_tag ("td") || die;
	    $parse->get_tag ("td") || die;
	    $rej = $parse->get_trimmed_text("/td") || die;
	}

	$parse->get_tag ("tr") || die; # Message Excerpt _or_ Headers
	$parse->get_tag ("td") || die;
	$parse->get_tag ("td") || die;
	$excerpt = $parse->get_text("/td");
	$spamscore = 0;
	$spamscore = length ($1)
		if $excerpt =~ /^X-UiO-Spam-score: (s+)/m;
	$date = "<no date>";
	$date = $1 if $excerpt =~ /^Date: (.*)$/m;

	if ($mailmanversion == 2) {
	    $parse->get_tag ("tr") || die;  # Message Excerpt
	    $parse->get_tag ("td") || die;
	    $parse->get_tag ("td") || die;
	    $excerpt .= "\n" . $parse->get_text("/td");
	}

	$parse->get_tag ("/table") || die;

	$data{$id} = { "from" => $from,
		       "subject" => $subject,
		       "date" => $date,
		       "reason" => $reason,
		       "spamscore" => $spamscore,
		       "rejreason" => $rej,
		       "excerpt" => $excerpt };
    }
    if ($mailmanversion == 1) {
	$data{"global"}{"actions"} = { "a" => 0,
				       "r" => 1,
				       "d" => 2 };
    } else {
	$data{"global"}{"actions"} = { "a" => 1,
				       "r" => 2,
				       "d" => 3 };
    }
    return \%data;
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
 printf "default discard\r\n";
 printf "# uncomment the following to get a terse transaction log\r\n";
 printf "# log \"~/.listadmin.log\"\r\n";
 printf "\r\n"
 for l in $LISTS; do printf "$l\r\n"; done
END
    system $cmd;
}

sub read_config {
    my ($file) = @_;

    my ($user, $pw, $spam, $list);
    my %conf = ();
    my $line = "";
    my $action = "";
    my $default = "";
    my $count = 0;
    my $lineno = 0;
    my $logfile;
    my $confirm = 1;
    
    my %act = ("approve" => "a", "discard" => "d",
	       "reject" => "r", "skip" => "s", "none" => "");


    return undef unless open (CONF, $file);
    while (<CONF>) {
	++$lineno;
	chomp;
	s/\r$//;
	next if /^\s*#/;
	if (/\\$/) {
	    $line = $`; # $PREFIX
	    next;
	}
	$line .= $_;
	$line =~ s/^\s+//;
	next if /^$/;
	if ($line =~ /^username\s+/i) {
	    $user = $'; # $POSTFIX
	    $user =~ s/\s+$//;
	    $user =~ s/^"(.*)"$/$1/;
	    if ($user !~ /^[a-z0-9.-]+\@[a-z0-9.-]+$/) {
		print STDERR "$file:$lineno: Illegal username: '$user'\n";
		exit 1;
	    }
	} elsif ($line =~ /^password\s+/i) {
	    $pw = $'; # $POSTFIX
	    $pw =~ s/\s+$//;
	    if ($pw =~ /^"(.*)"$/) {
		$pw = $1;
		$pw =~ s/\\"/"/g;
		$pw =~ s/\\\\/\\/g;
	    }
	} elsif ($line =~ /^spamlevel\s+/i) {
	    $spam = $';
	    if ($spam =~ /^(\d+)\s*$/) {
		$spam = $1;
	    } else {
		print STDERR "$file:$lineno: Illegal spamlevel value: '$spam'\n";
		exit 1;
	    }
	} elsif ($line =~ /^confirm\s+/i) {
	    $confirm = $'; # $POSTFIX
	    $confirm =~ s/^"(.*)"\s*/$1/;
	    if ($confirm eq "yes") {
		$confirm = 1;
	    } elsif ($confirm eq "no") {
		$confirm = undef;
	    } else {
		print STDERR "$file:$lineno: Illegal value: '$confirm'\n";
		exit 1;
	    }
	    $action = $act{$action};
	} elsif ($line =~ /^action\s+/i) {
	    $action = $'; # $POSTFIX
	    $action =~ s/^"(.*)"\s*/$1/;
	    unless (defined $act{$action}) {
		print STDERR "$file:$lineno: Illegal action value: '$action'\n";
		exit 1;
	    }
	    $action = $act{$action};
	} elsif ($line =~ /^default\s+/i) {
	    $default = $'; # $POSTFIX
	    $default =~ s/^"(.*)"\s*/$1/;
	    unless (defined $act{$default}) {
		print STDERR "$file:$lineno: Illegal default value: '$default'\n";
		exit 1;
	    }
	    $default = $act{$default};
	} elsif ($line =~ /^log\s+/i) {
	    $logfile = $'; # $POSTFIX
	    $logfile =~ s/^"(.*)"\s*/$1/;
	    $logfile =~ s/\\"/"/g;
	    $logfile =~ s/\\\\/\\/g;
	    $logfile =~ s,^\$HOME/,$ENV{'HOME'}/,;
	    $logfile =~ s,^~/,$ENV{'HOME'}/,;
	    $logfile =~ s,^~(\w+)/,(getpwnam($1))[7]."/",e;
	    if ($logfile =~ /^M:/i) {
		$logfile =~ s,\\,/,g;
		$logfile =~ s,^M:,$ENV{'HOME'},;
	    }
	    $logfile = undef if $logfile eq "none";
	} elsif ($line =~ /^([^@ \t]+@[^@])+\s*/) {
	    $conf{$line} = { "user" => $user,
			     "password" => $pw,
			     "spamlevel" => $spam,
			     "confirm" => $confirm,
			     "action" => $action,
			     "default" => $default,
			     "logfile" => $logfile,
			     "order" => ++$count,
			 };
	} else {
	    print STDERR "$file:$lineno: Syntax error: '$line'\n";
	    exit 1;
	}
	$line = "";
    }
    close (CONF);
    return \%conf;
}

sub prompt_for_config {
    my ($rc) = @_;

    print "No configuration file found: $rc\n";
    my $ans = $term->readline ("Do you want to create one? [yes] ");
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
    my $user = $term->readline ("Enter Mailman username: ");
    print "\n";
    print RC "username $user\r\n";
    my $pass = $term->readline ("Enter Mailman password (will appear on screen): ");
    print "\n";
    $pass =~ s/"/\\"/g;
    print RC "password \"$pass\"\r\n";

    print <<END;
Listadmin can discard messages with a high spam score automatically.
A value in the interval 5 to 12 is recommended.
END
    my $spam = $term->readline ("What threshold do you want? [8]");
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
	$list = $term->readline ("> ");
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
    my ($list, $user, $pw, $change, $msgs, $logfile) = @_;

    my $baseurl = mailman_url ($list, $user, $pw);
    my $action = $msgs->{"global"}{"actions"};
    my $changes = 0;

    my $log = log_timestamp ($list);
    my $url = $baseurl;

    for $id (keys %{$change}) {
	my ($what, $text) = @{$change->{$id}};
	$url .= "&$id=" . $action->{$what};
	$log .= sprintf ("%s D:[%s] F:[%s] S:[%s]\n",
			 $what,
			 $msgs->{$id}{"date"},
			 $msgs->{$id}{"from"},
			 $msgs->{$id}{"subject"});
	if ($what == "r") {
	    $text =~ s/(\W)/sprintf("%%%02x", ord($1))/ge;
	    $url .= "&comment_$id=$text";
	}
	++$changes;

	# HTTP does not specify a maximum length for the URI in a GET
	# request, but it recommends that a server does not rely on
	# clients being able to send URIs larger than 255 octets.  the
	# reject reason can be very long, so theoretically, we can
	# overshoot that limit.  Mailman has been observed to reject
	# URI's ~3400 octets long, but accept 6017.

	if (length ($url) > 2000) {
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
    my $ret = get ($url);
    if ($opened) {
	if ($ret) {
	    print LOG "changes sent to server";
	} else {
	    print LOG "server returned error";
	}
	print LOG " (URI length ", length ($url), ")\n";
	close (LOG);
    }
}
