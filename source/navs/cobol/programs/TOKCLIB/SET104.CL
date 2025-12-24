/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    データセーブ用ＦＰＤ作成             *  ./
/. *   JOB-ID      :    SET104                               *  ./
/. *   JOB-NAME    :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./

    ?OPR1  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　　新しいオンラインデータ退避用ＦＰＤセット　　　'
    ?OPR3  :=  '　　して下さい。！！　　　　　　　　　　　　　　　'
    ?OPR4  :=  '　　確認して下さい。　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    INZFPD  @WSFPD
    CRTFPF FILE-CVCSK104,DEV-@WSFPD,SIZE-950!@KB

    RETURN
