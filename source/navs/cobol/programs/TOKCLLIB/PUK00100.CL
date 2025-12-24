/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携　　　　　               *  ./
/. *   JOB-ID      :    PUK00100                             *  ./
/. *   JOB-NAME    :    Ｄ３６５計上データ確認用ＤＴ作成　　 * ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PUK00100'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門CD./
    VAR       ?TANTO    ,STRING*8,VALUE-'        ' /.担当者CD./
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./
    VAR       ?PARA1  ,STRING*8,VALUE-'00000000'
    VAR       ?PARA2  ,STRING*8,VALUE-'00000000'
    VAR       ?PARA3  ,STRING*1,VALUE-' '
    VAR       ?PARA4  ,STRING*1,VALUE-' '
    VAR       ?PARA5  ,STRING*1,VALUE-' '

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'Ｄ３６５計上データ再作成処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL LIBL-TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO/TOKDTLIB/
            D365DLIB/TOKWLIB/TOKMDLIB

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

SUK0010I: /.##計上ＤＴ抽出条件指定画面##./

    ?STEP :=   'SUK0010I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIBO,MEDLIB-TOKMDLIB
    CALL      PGM-SUK0010I.TOKSOLIB,
              PARA-(?PARA1,?PARA2)
    IF        @PGMEC    ^=   0  THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 := '計上ＤＴ抽出条件指定画面'
              GOTO  ABEND
         END
    END
    ?MSGX :=  '*** 開始日付 = '  && ?PARA1  &&   ' ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '*** 終了日付 = '  && ?PARA2  &&   ' ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

SUK0020B:  /.##計上ＤＴ抽出処理##./

    ?STEP :=   'SUK0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-SUK0020B.TOKSOLIB,
              PARA-(?PARA1,?PARA2)
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 := '計上ＤＴ抽出処理'
              GOTO  ABEND
    END

PCNVDF:  /.##抽出計上データ転送##./

    ?STEP :=   'PCNVDF  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVDF FILE-KEIURIDT.TOKDTLIB,
          PATH-'/HGNAS/KEIJYODT/KEIJYODT.CSV',MODE-@EXP,
          ADD-@NO,ACTCHK-@NO,EXTCNV-@YES
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 := '抽出計上データ転送'
              GOTO  ABEND
    END

RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '＃＃日次計上データ抽出処理＃＃'
    ?KEKA2 :=  '　処理が正常終了しました。'
    ?KEKA3 :=  '　件数を確認して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '＃＃日次計上データ抽出処理＃＃'
    ?KEKA2 :=  '　処理が異常終了しました。ログを'
    ?KEKA3 :=  '　採取し、管理者に連絡した下さい'
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
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

    RETURN    PGMEC-@PGMEC
