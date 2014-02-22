:Namespace Emacs
    transtable←0 8 10 13 32 12 6 7 27 9 9014 14 37 39 9082 9077 95 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 1 2 175 46 9068 48 49 50 51 52 53 54 55 56 57 3 8866 165 36 163 162 8710 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 4 5 253 183 127 9049 9398 9399 9400 9401 9402 9403 9404 9405 9406 9407 9408 9409 9410 9411 9412 9413 9414 9415 9416 9417 9418 9419 9420 9421 9422 9423 123 8364 125 8867 9015 168 192 196 197 198 9064 201 209 214 216 220 223 224 225 226 228 229 230 231 232 233 234 235 237 238 239 241 91 47 9023 92 9024 60 8804 61 8805 62 8800 8744 8743 45 43 247 215 63 8714 9076 126 8593 8595 9075 9675 42 8968 8970 8711 8728 40 8834 8835 8745 8746 8869 8868 124 59 44 9073 9074 9042 9035 9033 9021 8854 9055 9017 33 9045 9038 9067 9066 8801 8802 243 244 246 248 34 35 30 38 8217 9496 9488 9484 9492 9532 9472 9500 9508 9524 9516 9474 64 249 250 251 94 252 96 166 182 58 9079 191 161 8900 8592 8594 9053 41 93 31 160 167 9109 9054 9059
    :Namespace editor
        eomraw←27
        eom←⎕UCS eomraw
        recvbuf←⍬
        state←'ready'
        onMissing←''

        ∇ {r}←edit name;src
          src←getsource name
          r←send #.⎕SE.Emacs∆socket('edit ',name,' ',src,eom)
        ∇

        ∇ setupmenu shortcut;title;acc
          title acc←2↑shortcut,'Ctrl+Alt+Enter'(13 6)
          '⎕SE.popup.emacs'⎕WC'MenuItem'('Caption'('Edit in Emacs',⎕AV[10],title))
          '⎕SE.popup.emacs'⎕WS'Event' 'Select' '#.Emacs.editor.sessionedit'
          '⎕SE.popup.emacs'⎕WS'Accelerator'acc
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

        ∇ {r}←send args;socket;text;raw
          socket text←args
          raw←##.text2bytes text
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

        ∇ {r}←fix args;socket;raw;marker;src;header
          socket raw marker←args
          src←##.bytes2text recvbuf,raw[⍳marker-1]
          header←##.tolower src[⍳512⌊⊃⍴src]
          :If ∨/':class'⍷header
          :OrIf ∨/':namespace'⍷header
              r←#.⎕FIX ##.splitlines src
          :Else
              r←#.⎕FX↑##.splitlines src
          :EndIf
          send socket('fxresult ',(,⍕r),eom)
        ∇

        ∇ {r}←sendsource args;socket;raw;marker;name;src
          socket raw marker←args
          name←##.bytes2text recvbuf,raw[⍳marker-1]
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

        ∇ src←{noload}getsource name;_
          :If 0=⎕NC'noload'
              noload←0
          :EndIf

          :Select ⊃#.⎕NC name
          :Case 0
              :If 3≠⎕NC '#.',onMissing
              :OrIf noload
                  src←''
              :Else
                  _←(#.⍎'#.',onMissing) name
                  src←1 getsource name
              :EndIf
          :CaseList 3 4
              src←##.joinlines ##.cm2v #.⎕CR name
          :Case 9
              src←##.joinlines #.⎕SRC(⍎name)
          :Else
              src←''
          :EndSelect
        ∇
    :EndNamespace

    :Namespace session
        cr←⎕UCS 13
        lf←⎕UCS 10

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

        ∇ {r}←send args;socket;text;raw
          socket text←args
          raw←##.text2bytes text
          r←2 ⎕NQ socket'TCPSend'raw
        ∇

        ∇ {r}←receive msg;socket;raw;ip;data;z;prompt;dm;err;stack;cursor
         
          socket raw ip←msg[1 3 4]
         
          :If ip≢'127.0.0.1'
              :Return
          :EndIf
         
          data←##.bytes2text raw
          prompt←6⍴' '
          data←(-+/(¯2↑data)∊cr lf)↓data
         
          :If data∧.=' ' ⍝ An empty input line
          :OrIf ∧/(data=' ')∨∨\data='⍝' ⍝ A comment
              send socket prompt
              :Return
          :EndIf
         
          :Trap 0
              z←#.⍎data
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
              dm←⎕DM
              err←(('⍎'=⊃⊃dm)∧~'⍎'∊data)↓1⊃dm
              cursor←3⊃dm
              :If 'receive[19] '≡12↑stack←2⊃dm
                  stack←prompt,12↓stack
                  cursor←6↓cursor
              :EndIf
              send socket(##.joinlines err stack cursor prompt)
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

    listen←{
        sessionport editorport←2↑⍵,7979 8080
        a←session.listen sessionport
        a,editor.listen editorport
    }

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

    text2bytes←{
        ⎕AVU←transtable
        'UTF-8'⎕UCS ⍵
    }

    bytes2text←{
        ⎕AVU←transtable
        'UTF-8'⎕UCS ⍵
    }

:EndNamespace