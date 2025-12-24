/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PSE00801                             *  ./
/. *   JOB-NAME    :    更新                                 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSE00801'
    VAR       ?STEP     ,STRING*8
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./
/.##請求締処理開始メッセージ##./
STEP00:
    ?OPR1  :=  '　＃＃＃＃＃＃＃　請求締更新処理　＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　ホーマックグループの請求締更新処理を行います。'
    ?OPR3  :=  '　確認して下さい。'
    ?OPR4  :=  '　支払照合までに必ず完了して下さい。'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
     DEFLIBL   TOKFLIB/TOKELIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ホーマックグループ請求締更新'

/.##ホーマック関東繰越##./
PKURIK10:

    ?STEP :=   'PKURIK10'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

 /.ホーマック関東抽出./
 /. SORTF     INFILE-SETGKFA.TOKFLIB,
              OUTFILE-SETGKFH.TOKFLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00000880',
              RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ホーマック関東　請求ＤＴ抽出】'
              GOTO ABEND END
   ./
/.##ホーマーＧ関東繰越##./
PKURIK20:

    ?STEP :=   'PKURIK20'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

 /.ホーマーＧ関東抽出./
 /. SORTF     INFILE-SETGKFA.TOKFLIB,
              OUTFILE-SEIGK39.TOKFLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00000881',
              RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ホーマーＧ関東　請求ＤＴ抽出】'
              GOTO ABEND END
   ./
/.##仙台ホーマーＧ繰越##./
PKURIK30:

    ?STEP :=   'PKURIK30'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-SETGKFN.TOKFLIB,TOFILE-SETGKFN.TOKWLIB,
          ADD-@NO,ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【仙台ホーマーＧ繰越】'
              GOTO ABEND END

/.##仙台ホーマーＧ初期化##./
PKURIK40:

    ?STEP :=   'PKURIK40'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-SETGKFN.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【仙台ホーマーＧ初期化】'
              GOTO ABEND END

/.##仙台ホーマック繰越##./
PKURIK50:

    ?STEP :=   'PKURIK50'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-SETGKFO.TOKFLIB,TOFILE-SETGKFO.TOKWLIB,
          ADD-@NO,ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【仙台ホーマック繰越】'
              GOTO ABEND END

/.##仙台ホーマーＧ初期化##./
PKURIK60:

    ?STEP :=   'PKURIK60'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-SETGKFO.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【仙台ホーマック初期化】'
              GOTO ABEND END

/.##北海道ホーマック繰越##./
PKURIK70:

    ?STEP :=   'PKURIK70'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-SETGKFR.TOKFLIB,TOFILE-SETGKFR.TOKWLIB,
          ADD-@NO,ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【北海道ホーマック繰越】'
              GOTO ABEND END

/.##北海道ホーマック初期化##./
PKURIK80:

    ?STEP :=   'PKURIK80'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-SETGKFR.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【北海道ホーマック初期化】'
              GOTO ABEND END

/.##北海道ホーマーＧ繰越##./
PKURIK90:

    ?STEP :=   'PKURIK90'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-SETGKFQ.TOKFLIB,TOFILE-SETGKFQ.TOKWLIB,
          ADD-@NO,ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【北海道ホーマーＧ繰越】'
              GOTO ABEND END

/.##北海道ホーマーＧ初期化##./
PKURIK95:

    ?STEP :=   'PKURIK95'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-SETGKFQ.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【北海道ホーマーＧ初期化】'
              GOTO ABEND END

RTN:

    ?KEKA1 :=  'ホーマックＧの請求締更新が終了しました。'
    ?KEKA2 :=  '更新結果を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '請求締更新が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
