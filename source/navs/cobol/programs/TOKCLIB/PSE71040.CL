/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    本田オンラインシステム               *  ./
/. *   JOB-ID      :    PSE71040                             *  ./
/. *   JOB-NAME    :    配信ファイル作成（ジョイフル本田）   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSE71040'
    VAR       ?STEP     ,STRING*8
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGNM     ,STRING*24!MIXED            /.ＰＧ漢字名 ./
    VAR       ?PGID     ,STRING*10                  /.ＰＧＩＤ　 ./
/.-----------------------------------------------------------./
    VAR       ?CLID     ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
/.  本田請求送信データ作成画面起動                          ./
STEP00:
    ?JBID    :=   'PSE71040'
    ?CLID    :=   'PSE71040'
    ?MSG1    := '<<< ' && ?CLID && ' START '
               && %SBSTR(@SDATEY,1,2) && '/'
               && %SBSTR(@SDATEY,3,2) && '/'
               && %SBSTR(@SDATEY,5,2) && ' '
               && %SBSTR(@STIME,1,2)  && ':'
               && %SBSTR(@STIME,3,2)  && ':'
               && %SBSTR(@STIME,5,2)  && ' >>>'
     SNDMSG  MSG-?MSG1,TOWS-@ORGWS,JLOG-@YES

    ?OPR1  :=  '＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    ?OPR2  :=  '　　本田殿請求配信データの作成を行います。'
    ?OPR3  :=  '　確認して下さい。'
    ?OPR4  :=  '　本田殿請求用ＦＰＤをセットしてください。'
    ?OPR5  :=  '＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL



/.##配信ファイル作成##./
SSE7104B:

    ?STEP :=   'SSE7104B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JDJSEKL1,TOFILE-JDJSEKL1.TOKKLIB
    OVRF      FILE-CVCSF,TOFILE-JOYFUL.TOKKLIB
    CALL      PGM-SSE7104B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##退避処理（請求データ）##./
CNVFILE:

    ?STEP :=   'CNVFILE '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  ?OPR1  :=  '　＃＃＃＃＃＃　山新殿請求データ作成　＃＃＃＃＃＃'
    ?OPR2  :=  '　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR3  :=  '　請求データ退避用のＦＰＤをセットして下さい。'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)./

/.##############################################################./
    ?MSGX := '## 請求ﾃﾞｰﾀ退避中（本田） ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    CNVFILE FILE-JOYFUL.TOKKLIB,TOFILE-JOYFUL,DEV-@WSFPD,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END


    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',TO-XCTL
    SNDMSG MSG-'＃　本田殿請求配信データの作　＃',TO-XCTL
    SNDMSG MSG-'＃成処理が完了しました。　　　＃',TO-XCTL
    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',TO-XCTL

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

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