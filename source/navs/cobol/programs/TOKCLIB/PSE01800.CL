/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ホーマック関東オンラインシステム     *  ./
/. *   JOB-ID      :    PSE01800                             *  ./
/. *   JOB-NAME    :    配信ファイル作成（ホーマック関東）   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSE01800'
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
/.  ホーマック請求送信データ作成画面起動                        ./
STEP00:
    ?JBID    :=   'PSE01800'
    ?CLID    :=   'PSE01800'
    ?MSG1    := '<<< ' && ?CLID && ' START '
               && %SBSTR(@SDATEY,1,2) && '/'
               && %SBSTR(@SDATEY,3,2) && '/'
               && %SBSTR(@SDATEY,5,2) && ' '
               && %SBSTR(@STIME,1,2)  && ':'
               && %SBSTR(@STIME,3,2)  && ':'
               && %SBSTR(@STIME,5,2)  && ' >>>'
     SNDMSG  MSG-?MSG1,TOWS-@ORGWS,JLOG-@YES

    ?OPR1  :=  '　ホーマック関東殿請求配信データの作成を行います'
    ?OPR2  :=  '　確認して下さい。'
    ?OPR3  :=  '　また，作成完了後，Ｋ６５００側へデータを転送し'
    ?OPR4  :=  '、最終配信データ作成処理を行います。その後，Ｋ６'
    ?OPR5  :=  '　５００側より配信処理を行って下さい。'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL



/.##配信ファイル作成##./
SSE0180B:

    ?STEP :=   'SSE0180B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SETGKFA2,TOFILE-SETGKFA2.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-CVCSF,TOFILE-SEIWK.TOKFLIB
    CALL      PGM-SSE0180B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##請求データ送信（ＧＰ－＞Ｋ６５００）##./
PSNDFILE:

    ?STEP :=   'PSNDFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃＃ホーマック　請求Ｆ転送　開始＃',TO-XCTL
    SNDFILE COM-KGPATH01.KGNODE01,SFILE-SEIWK.TOKFLIB,
            DFILE-'SEIWK',RL-256,BF-1
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'＃＃ホーマック　請求Ｆ転送　異常＃',TO-XCTL
              GOTO ABEND
    ELSE
              SNDMSG MSG-'＃＃ホーマック　請求Ｆ転送　終了＃',TO-XCTL
    END

/.##Ｋ６５００側ジョブ起動##./
PSNDRSJJ:

    ?STEP :=   'PSNDRSJJ'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃＃Ｋ６５００側処理起動　開始＃',TO-XCTL
    SNDRSJJ COM-KGPATH01.KGNODE01,DFILE-'HOMACHS'
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'＃＃Ｋ６５００側処理起動　異常＃',TO-XCTL
              GOTO ABEND
    ELSE
              SNDMSG MSG-'＃＃Ｋ６５００側処理起動　終了＃',TO-XCTL
    END

    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',TO-XCTL
    SNDMSG MSG-'＃　ホーマック関東殿請求配信データの＃',TO-XCTL
    SNDMSG MSG-'＃作成処理が完了しました。　　　　　＃',TO-XCTL
    SNDMSG MSG-'＃現在，Ｋ６５００側にて最終処理中で＃',TO-XCTL
    SNDMSG MSG-'＃す。最終処理が完了次第Ｋ６５００側＃',TO-XCTL
    SNDMSG MSG-'＃にて，ホーマック関東へのデータ送信＃',TO-XCTL
    SNDMSG MSG-'＃処理を開始して下さい。　　　　　　＃',TO-XCTL
    SNDMSG MSG-'＃　　　　　　　　　　　　　　　　　＃',TO-XCTL
    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',TO-XCTL

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