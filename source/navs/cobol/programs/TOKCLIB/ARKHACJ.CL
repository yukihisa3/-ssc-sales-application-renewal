/.  ********************************************************    ./
/.  *    _サカタのタネ　特販部　手動受信システム          *    ./
/.  *    取引先：アークランドサカモト（発注）              *    ./
/.  *    作成者：NAV T.TAKAHASHI                           *    ./
/.  *    作成日：02/01/08                   ID:ARKHACJ     *    ./
/.  ********************************************************    ./
    PGM
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGNM   ,STRING*24!MIXED            /.ＰＧ漢字名 ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
/.-----------------------------------------------------------./
    VAR       ?CLID   ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1   ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./
/.  手動受信処理　起動確認メッセージ                            ./
STEP00:
    ?JBID    :=   'ARKHACJ '
    ?CLID    :=   'ARKHACJ '
    ?MSG1    := '<<< ' && ?CLID && ' START '
               && %SBSTR(@SDATEY,1,2) && '/'
               && %SBSTR(@SDATEY,3,2) && '/'
               && %SBSTR(@SDATEY,5,2) && ' '
               && %SBSTR(@STIME,1,2)  && ':'
               && %SBSTR(@STIME,3,2)  && ':'
               && %SBSTR(@STIME,5,2)  && ' >>>'
     SNDMSG  MSG-?MSG1,TOWS-@ORGWS,JLOG-@YES

    ?OPR1  :=  '　＃＃＃＃＃　手　動　受　信　処　理　＃＃＃＃＃　'
    ?OPR2  :=  '　取引先：アークランドサカモト（発注データ）　　　'
    ?OPR3  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    ?OPR4  :=  '　　　　　受信処理を開始して良いですか？　　　　　'
    ?OPR5  :=  '　　　　　確認して下さい。　　　　　　　　　　　　'
    CALL      OHOM0900.SKTELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
/.  受信処理開始                                                ./
STEP01:
    SNDMSG MSG-'＃＃手動受信開始＃＃',TOWS-@ORGWS,JLOG-@YES
    SBMJOB JOB-ARKHACJ,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-PARKHAC.TOKELIB,LIBL-TOKELIB

    RETURN    PGMEC-@PGMEC
