/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PSE00800                             *  ./
/. *   JOB-NAME    :    更新                                 *  ./
/. *   2013/05/22 NAV TAKAHASHI                              *  ./
/. *   イオン分消込を行なう為、イオン本社、九州分処理は停止  *  ./
/. *   2017/04/18 NAV TAKAHASHI                              *  ./
/. *   Ｊ本田分ファイル店舗４桁対応（ファイル変更）　　　　  *  ./
/. *   2023/08/04 NAV TAKAHASHI                              *  ./
/. *   セキチュー繰越追加　　　　　　　　　　　　　　　　　  *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSE00800'
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
    ?OPR2  :=  '　ケーヨー以外の請求締更新処理を行います。'
    ?OPR3  :=  '　確認して下さい。'
    ?OPR4  :=  ''
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
     DEFLIBL   TOKFLIB/TOKELIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ケーヨー以外請求締更新'

/.##山新繰越##./
PSORTF1:

    ?STEP :=   'PSORTF1 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORTF     INFILE-SETGKFA.TOKFLIB,     /.##ジョイフル山新抽出##./
              OUTFILE-SETGKFG.TOKFLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00001041',
              RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【山新繰越】'
              GOTO ABEND END

/.##ビーバー登山繰越##./
PSORTF2:

    ?STEP :=   'PSORTF2 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  SORTF     INFILE-SETGKFA.TOKFLIB,       ##ビーバー登山抽出##
              OUTFILE-SETGKFK.TOKFLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00006002',
              RCDL-@DSP                     ./
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ビバー登山繰越】'
              GOTO ABEND END

/.##ロイヤルＨＣ繰越##./
PSORTF3:

    ?STEP :=   'PSORTF3 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORTF     INFILE-SETGKFA.TOKFLIB,     /.##ロイヤルＨＣ抽出##./
              OUTFILE-SETGKFT.TOKKLIB,
              KEY-'F01!A',
              SELECT-'F01!EQ!#00051649',
              RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ロイヤルＨＣ繰越】'
              GOTO ABEND END

/.##仙台イオン繰越##./
PKURIK30:

    ?STEP :=   'PKURIK30'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-SETGKFP.TOKFLIB,TOFILE-SETGKFP.TOKWLIB,
          ADD-@NO,ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【仙台イオン繰越】'
              GOTO ABEND END

/.##仙台イオン初期化##./
PKURIK31:

    ?STEP :=   'PKURIK31'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  CLRFILE FILE-SETGKFP.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【仙台イオン初期化】'
              GOTO ABEND END
./
/.##トステムビバ繰越##./
PKURIK40:

    ?STEP :=   'PKURIK40'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-SEIGK50.TOKFLIB,TOFILE-SEIGK50.TOKWLIB,
          ADD-@NO,ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【トステムビバ繰越】'
              GOTO ABEND END

/.##トステムビバ初期化##./
PKURIK41:

    ?STEP :=   'PKURIK41'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-SEIGK50.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【トスエムビバ初期化】'
              GOTO ABEND END

/.##Ｊ本田繰越##./
PKURIK70:

    ?STEP :=   'PKURIK70'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-JDSEIKF.TOKKLIB,TOFILE-JDSEIKF.TOKWLIB,
          ADD-@NO,ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ジョイフル本田繰越】'
              GOTO ABEND END

/.##Ｊ本田初期化##./
PKURIK71:

    ?STEP :=   'PKURIK71'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-JDSEIKF.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【Ｊ本田初期化】'
              GOTO ABEND END

/.##2017/04/19 NAV ST##./
/.##Ｊ本田繰越##./
PKURIK72:

    ?STEP :=   'PKURIK72'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-JHSEIKF.TOKDLIB,TOFILE-JHSEIKF.TOKWLIB,
          ADD-@NO,ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ジョイフル本田新　繰越】'
              GOTO ABEND END

/.##Ｊ本田初期化##./
PKURIK73:

    ?STEP :=   'PKURIK73'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-JHSEIKF.TOKDLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【Ｊ本田新　初期化】'
              GOTO ABEND END
/.##2017/04/19 NAV ED##./

/.##イオン九州ＨＷ繰越##./
PKURIK80:

    ?STEP :=   'PKURIK80'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-SEIGK46.TOKFLIB,TOFILE-SEIGK46.TOKWLIB,
          ADD-@NO,ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【イオン九州ＨＷ繰越】'
              GOTO ABEND END

/.##イオン九州ＨＷ初期化##./
PKURIK81:

    ?STEP :=   'PKURIK81'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  CLRFILE FILE-SEIGK46.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【イオン九州ＨＷ初期化】'
              GOTO ABEND END
./
/.##セキチュー繰越##./
PKURIK90:

    ?STEP :=   'PKURIK90'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-SETGKFE.TOKFLIB,TOFILE-SETGKFE.TOKWLIB,
          ADD-@NO,ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【セキチュー繰越】'
              GOTO ABEND END

/.##セキチュー初期化##./
PKURIK91:

    ?STEP :=   'PKURIK91'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-SETGKFE.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【セキチュー初期化】'
              GOTO ABEND END

RTN:

    ?KEKA1 :=  'ケーヨー以外の請求締更新が終了しました。'
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