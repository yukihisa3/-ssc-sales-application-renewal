/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    アークランドオンラインシステム       *  ./
/. *   JOB-ID      :    HSARKSND                             *  ./
/. *   JOB-NAME    :    アークランド欠品案内データ送信       *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

/.##送信確認メニュー##./
STEP00:

    ?OPR1  :=  '　＃＃＃＃＃　欠品案内データ配信処理　＃＃＃＃＃　'
    ?OPR2  :=  '　アークランドサカモト殿の欠品データを送信します。'
    ?OPR3  :=  '　確認して下さい。　　　　　　　　　　　　　　　　'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    SBMJOB JOB-HSARKSND,JOBD-CVCS.XUCL,JOBK-@B,
           VSIZE-6,RSIZE-16,
           PGM-CVCS1001.CVCSOBJ,LIBL-CVCSLBI/CVCSOBJ,
           PARA-('01BB63410130Z')

    RETURN    PGMEC-@PGMEC
