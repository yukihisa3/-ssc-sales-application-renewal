# KEYOFPD

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/KEYOFPD.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ケーヨー支払ＦＰＤ作成               *  ./
/. *   JOB-ID      :    KEYOFPD                              *  ./
/. *   JOB-NAME    :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./

    ?OPR1  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　　新しいケーヨー支払ＦＰＤをセットして下さい。　'
    ?OPR3  :=  '　　確認して下さい。　　　　　　　　　　　　　　　'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    INZFPD  @WSFPD
    CRTFPF FILE-KEYO,DEV-@WSFPD,SIZE-950!@KB

    RETURN

```
