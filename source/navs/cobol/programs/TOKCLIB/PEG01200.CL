/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    営業所データ連携                     *  ./
/. *   JOB-ID      :    PZA00502                             *  ./
/. *   JOB-NAME    :    大阪営業所在庫マスタ抽出             *  ./
/. ***********************************************************  ./
    PGM
    VAR ?PGMEC    ,INTEGER
    VAR ?PGMECX   ,STRING*11
    VAR ?PGMEM    ,STRING*99
    VAR ?MSG      ,STRING*99(6)
    VAR ?MSGX     ,STRING*99
    VAR ?PGMID    ,STRING*8,VALUE-'PEG01200'
    VAR ?STEP     ,STRING*8
    VAR ?WKSTN    ,STRING*8
    VAR ?OPR1     ,STRING*50                    /.ﾒｯｾｰｼﾞ1./
    VAR ?OPR2     ,STRING*50                    /.      2./
    VAR ?OPR3     ,STRING*50                    /.      3./
    VAR ?OPR4     ,STRING*50                    /.      4./
    VAR ?OPR5     ,STRING*50                    /.      5./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾒｯｾｰｼﾞ ##./
    ?OPR1  :=  '　＃＃＃＃＃＃＃　本社データ連携　＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　　本社抽出在庫マスタの取り込み処理を行います。　'
    ?OPR3  :=  '　　確認して下さい。　　　　　　　　　　　　　　　'
    ?OPR4  :=  '　　データは，全て上書きで更新されます。　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##受信在庫マスタコピー##./
PSETPF:

    ?STEP :=   'PSETPF  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX := '## 在庫ﾃﾞｰﾀｺﾋﾟｰ中   ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    SETPF FILE-EGYZAIF.TOKFLIB,TOFILE-ZZAIMS.TOKFLIB,ADD-@NO,
          ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '## 在庫ﾃﾞｰﾀｺﾋﾟｰ完了 ##'
    SNDMSG MSG-?MSGX,TO-XCTL

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