:Namespace Emacs

    :Namespace editor
        eomraw←27
        eom←⎕UCS eomraw
        recvbuf←⍬
        state←'ready'

        ∇ {r}←edit name;fullname;src
          fullname←(('#.'≢2↑name)/'#.'),name
          src←getsource fullname
          r←send #.⎕SE.Emacs∆socket('edit ',name,' ',src,eom)
        ∇

        ∇ setupmenu shortcut;acc
          acc←2↑shortcut,13 6
          '⎕SE.popup.emacs'⎕WC'MenuItem'('Caption'('Edit in Emacs',⎕AV[10],'Ctrl+Alt+Enter'))
          '⎕SE.popup.emacs'⎕WS'Event' 'Select' '#.Emacs.editor.sessionedit'
          '⎕SE.popup.emacs'⎕WS'Accelerator'(13 6)
        ∇

        ∇ {msg}←sessionedit msg;name
          name←'⎕SE'⎕WG'CurObj'
          edit name
        ∇

        ∇ {r}←listen port;sockname;callbacks;_
          sockname←'⎕SE.Emacs_socket',⍕port
          callbacks←⊂('Event' 'TCPAccept' '#.Emacs.editor.accept')
          callbacks,←⊂('Event' 'TCPRecv' '#.Emacs.editor.receive')
          callbacks,←⊂('Event' 'TCPError' '#.Emacs.editor.error')
          callbacks,←⊂('Event' 'TCPClose' '#.Emacs.editor.close')
          sockname ⎕WC'TCPSocket' ''(port)('Style' 'Raw'),callbacks ⍝⋄ _←⎕DQ'.'
          r←sockname
          #.⎕SE.Emacs∆socket←sockname
        ∇

        ∇ {r}←accept msg;socket;newname
          :If 0≠⎕NC #.⎕SE.Emacs∆socket
              ⍝ Another editor is already connected
              r←0
              :Return
          :EndIf
          socket←1⊃msg
          newname←socket,⍕?¯2+2*31
          newname ⎕WC'TCPSocket'('SocketNumber'(3⊃msg))('Event'(socket ⎕WG'Event'))
          r←newname
        ∇

        ∇ {r}←send args;socket;text;uni;raw;⎕PATH
          socket text←args
          ⎕PATH←'↑'
          uni←'Dyalog APL Source'#.ENCODINGS_DECODE text
          raw←¯1+⎕AV⍳'UTF-8'#.ENCODINGS_ENCODE uni
          r←2 ⎕NQ socket'TCPSend'raw
        ∇

        ∇ {r}←receive msg;socket;raw;ip;uni;data;cr;lf;i;command;src;name;marker;complete
         
          socket raw ip←msg[1 3 4]
         
          :If ip≢'127.0.0.1'
              :Return
          :EndIf
         
          marker←raw⍳eomraw
          complete←marker≤⊃⍴raw
         
          :Select state
          :Case 'ready'
              i←raw⍳'UTF-8'⎕UCS' '
              command←'UTF-8'⎕UCS raw[⍳i-1]
         
              :Select command
              :Case 'fx'
                  :If complete
                      fix socket(i↓raw)(marker-i)
                      recvbuf←⍬
                  :Else
                      recvbuf,←i↓raw
                      state←'fx'
                  :EndIf
              :Case 'src'
                  :If complete
                      sendsource socket(i↓raw)(marker-i)
                      recvbuf←⍬
                  :Else
                      recvbuf,←i↓raw
                      state←'src'
                  :EndIf
              :Else
                  ⎕←'Received invalid command: ',command
              :EndSelect
         
          :Case 'fx'
              :If complete
                  fix socket raw marker
                  state←'ready'
                  recvbuf←⍬
              :Else
                  recvbuf,←raw
              :EndIf
          :EndSelect
        ∇

        ∇ {r}←fix args;socket;raw;marker;src;uni;header
          socket raw marker←args
          src←recvbuf,raw[⍳marker-1]
          uni←'UTF-8'#.ENCODINGS_DECODE ⎕AV[src+⎕IO]
          src←'Dyalog APL Source'#.ENCODINGS_ENCODE uni
          header←##.tolower src[⍳512⌊⊃⍴src]
          :If ∨/':class'⍷header
          :OrIf ∨/':namespace'⍷header
              r←#.⎕FIX ##.splitlines src
          :Else
              r←#.⎕FX↑##.splitlines src
          :EndIf
          send socket('fxresult ',(,⍕r),eom)
        ∇

        ∇ {r}←sendsource args;socket;raw;marker;name;uni;src
          socket raw marker←args
          name←recvbuf,raw[⍳marker-1]
          uni←'UTF-8'#.ENCODINGS_DECODE ⎕AV[name+⎕IO]
          name←'Dyalog APL Source'#.ENCODINGS_ENCODE uni
          src←getsource name
          r←send socket('edit ',name,' ',src,eom)
        ∇

        ∇ {r}←close msg
          ⎕EX 1⊃msg
          r←1
        ∇

        ∇ {r}←error msg
          ∘
        ∇

        ∇ src←getsource name
          :Select ⊃#.⎕NC name
          :CaseList 3 4
              src←##.joinlines ##.cm2v #.⎕CR name
          :Case 9
              src←##.joinlines #.⎕SRC name
          :Else
              src←''
          :EndSelect
        ∇
    :EndNamespace

    :Namespace session
        cr←⎕UCS 13
        lf←⎕ucs 10

        ∇ {r}←listen port;sockname;callbacks;_
          sockname←'⎕SE.Emacs_socket',⍕port
          callbacks←⊂('Event' 'TCPAccept' '#.Emacs.session.accept')
          callbacks,←⊂('Event' 'TCPRecv' '#.Emacs.session.receive')
          callbacks,←⊂('Event' 'TCPError' '#.Emacs.session.error')
          callbacks,←⊂('Event' 'TCPClose' '#.Emacs.session.close')
          sockname ⎕WC'TCPSocket' ''(port)('Style' 'Raw'),callbacks ⍝⋄ _←⎕DQ'.'
          r←sockname
        ∇

        ∇ {r}←accept msg;socket;newname
          socket←1⊃msg
          newname←socket,⍕?¯2+2*31
          newname ⎕WC'TCPSocket'('SocketNumber'(3⊃msg))('Event'(socket ⎕WG'Event'))
          r←newname
          send socket(6⍴' ')
        ∇

        ∇ {r}←send args;socket;text;uni;raw;⎕PATH
          socket text←args
          ⎕PATH←'↑'
          uni←'Dyalog APL Source'#.ENCODINGS_DECODE text
          raw←¯1+⎕AV⍳'UTF-8'#.ENCODINGS_ENCODE uni
          r←2 ⎕NQ socket'TCPSend'raw
        ∇

        ∇ {r}←receive msg;socket;raw;ip;uni;data;z;prompt;⎕PATH
         
          socket raw ip←msg[1 3 4]
         
          :If ip≢'127.0.0.1'
              :Return
          :EndIf
         
          ⎕PATH←'↑'
          uni←'UTF-8'#.ENCODINGS_DECODE ⎕AV[⎕IO+raw]
          data←'Dyalog APL Source'#.ENCODINGS_ENCODE uni
          prompt←6⍴' '
          data←(-+/(¯2↑data)∊cr lf)↓data
         
          :If data∧.=' ' ⍝ An empty input line
              send socket prompt
              :Return
          :EndIf
         
          :Trap 0
              z←⍎data
              :If 3=⎕NC'z'
                  r←{(3+⍵⍳']')↓⍵},⍕⎕OR'z'
              :Else
                  r←⎕FMT z
              :EndIf
              :If '←'∊data ⍝ TODO: Better test for assignment
                  send socket prompt
              :Else
                  send socket((,(r,cr),lf),prompt)
              :EndIf
          :Else
              send socket((,((↑''prompt prompt,¨0 12 12↓¨⎕DM),cr),lf),prompt)
          :EndTrap
        ∇

        ∇ {r}←close msg
          ⎕EX 1⊃msg
          r←1
        ∇

        ∇ {r}←error msg
          ∘
        ∇
    :EndNamespace

      join←{
          0=⍴,⍵:⍵
          (-⍴,⍺)↓⊃,/⍵,¨⊂⍺
      }

      split←{
          p←⊃1↑⍵
          (1↓¨(1,⍺)⊂p,⍵)~⊂0⍴p
      }

      splitlines←{
          (⍵∊⎕UCS 10 13)split ⍵
      }

      joinlines←{
          (⎕UCS 13 10)join ⍵
      }

    cm2v←{(+/∨\' '≠⌽⍵)↑¨↓⍵}

      tolower←{
          s←⍵
          i←⎕A⍳s
          hits←i≤⊃⍴⎕A
          s[hits/⍳⍴s]←'abcdefghijklmnopqrstuvwxyz'[hits/i]
          s
      }

:EndNamespace