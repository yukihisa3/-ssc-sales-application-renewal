/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ホーマックオンラインシステム         *  ./
/. *   JOB-ID      :    PSE02310                             *  ./
/. *   JOB-NAME    :    配信ファイル作成（ホーマック仙台）　 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSE02310'
    VAR       ?STEP     ,STRING*8
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID     ,STRING*10                  /.ＰＧＩＤ　 ./
/.-----------------------------------------------------------./
    VAR       ?CLID     ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
/.-----------------------------------------------------------./
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ホーマック仙台　請求送信　前処理'

/.##ホーマック仙台請求送信データ作成確認##./
STEP00:
    ?JBID    :=   'PSE02310'
    ?CLID    :=   'PSE02310'
    ?MSG1    := '<<< ' && ?CLID && ' START '
               && %SBSTR(@SDATEY,1,2) && '/'
               && %SBSTR(@SDATEY,3,2) && '/'
               && %SBSTR(@SDATEY,5,2) && ' '
               && %SBSTR(@STIME,1,2)  && ':'
               && %SBSTR(@STIME,3,2)  && ':'
               && %SBSTR(@STIME,5,2)  && ' >>>'
     SNDMSG  MSG-?MSG1,TOWS-@ORGWS,JLOG-@YES

    ?OPR1  :=  '　ホーマック殿　請求データの作成を行います。　　　'
    ?OPR2  :=  '　確認して下さい。　　　　　　　　　　　　　　　　'
    ?OPR3  :=  '　また，作成完了後，メニューより送信を選択して　　'
    ?OPR4  :=  '　下さい。　　　（仙台分）　　　　　　　　　　　　'
    ?OPR5  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##配信ファイル作成##./
SSE0230B:

    ?STEP :=   'SSE0230B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SETGKFA2,TOFILE-SETGKFA2.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-CVCSF,TOFILE-SEIWK.TOKFLIB
    CALL      PGM-SSE0230B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【送信ＤＴ作成】'
              GOTO ABEND END

/.##配信ファイルＫ６５００へ送信##./
PSNDFILE:

    ?STEP :=   'PSNDFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDFILE COM-KGPATH01.KGNODE01,SFILE-SEIWK.TOKFLIB,
            DFILE-'SEIWK',RL-256,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ＧＰ側　送信処理】'
              GOTO ABEND END

/.##Ｋ６５００側　請求配信処理　起動##./
PSNDRSJJ:

    ?STEP :=   'PSNDRSJJ'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDRSJJ COM-KGPATH01.KGNODE01,DFILE-'HSSHOM'
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ＧＰ側ＪＯＢ　起動】'
              GOTO ABEND END

RTN:

    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '現在、Ｋ側にて送信前処理中です。'
    ?KEKA3 :=  'Ｋ側送信前処理が完了次第、送信願います。'
    ?KEKA4 :=  'ＣＶＣＳリストの＊＊を確認して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'この画面を印刷し、ＮＡＶへ連絡して下さい。'
    ?KEKA3 :=  '現状、送信処理は中止して下さい。'
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