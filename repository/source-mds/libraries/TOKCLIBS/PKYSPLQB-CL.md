# PKYSPLQB

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PKYSPLQB.CL`

## ソースコード

```jcl
/.*****************************************************************./
/.*      ＷＳログ出力制御処理                                     *./
/.*      MEMBER  NAME       PKYSPLQB                               *./
/.*                                                               *./
/.*****************************************************************./

    PGM   (P1-?NSPLF,P2-?NSIKI)

    PARA  ?NSPLF  ,STRING*08,IN,VALUE-'        '
    PARA  ?NSIKI  ,STRING*03,IN,VALUE-'   '

    VAR   ?P      ,STRING*99
    VAR   ?C      ,STRING*30,VALUE-'PKYSPLQB        JOB  END'
    VAR   ?D      ,STRING*3,VALUE-' - '
    VAR   ?PGMEC  ,INTEGER
    VAR   ?NSIKIS ,INTEGER
    VAR   ?NSPLFN ,NAME
    VAR   ?WKSTN  ,NAME
    VAR   ?PFROM  ,NAME
    VAR   ?PFROMN ,STRING*7,VALUE-'       '
    VAR   ?LNAME  ,NAME
    VAR   ?LNAME0 ,STRING*08,VALUE-'WSLOGDMP'
    VAR   ?LIBNM  ,STRING*8,VALUE-'WSLOGLIB'
    VAR   ?FILID  ,NAME
    VAR   ?LIBID  ,NAME
    VAR   ?WKSTNNM,NAME!MOD
    VAR   ?WKSTNMM,STRING*17
    VAR   ?MSGX   ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./

/.##ﾌｧｲﾙ名作成##./
    ?MSGX := '## 出力WS = ' && ?NSPLF && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?FILID    :=    %NAME(?NSPLF)
    ?LIBID    :=    %NAME(?LIBNM)
    ?WKSTNNM  :=    %NCAT(?FILID,?LIBID)
    ?WKSTNMM  :=    %STRING(?WKSTNNM)
    ?MSGX     :=    '## 出力ﾌｧｲﾙ名 = ' && ?WKSTNMM && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##名前型へ変換##./
    ?NSPLFN := %NAME(?NSPLF)
    ?NSIKIS := %INTEGER(?NSIKI)

/.##ｽﾌﾟｰﾙﾌｧｲﾙｺﾋﾟｰ##./
 RUN1:

    CNVSPLF FILE-?NSPLFN!?NSIKIS,JOB-*.*,SPLQ-XXLOGQ,
            ESPLF-?WKSTNNM,REP-@YES
    IF  @PGMEC ^= 0  THEN
        ?MSGX := '## CNVSPLF ABEND ' && ?WKSTNMM && ' ##'
        SNDMSG ?MSGX,TO-XCTL
    END

 OWARIXX:
    ?PGMEC    :=  @PGMEC
    ?C        :=  ?C   &&   ?D
    IF        ?PGMEC   ^=   0   THEN
              ?P    :=   ?C   &&   @PGMEM
    ELSE
              ?P    :=   ?C                           END
    CHGCMVAR  CMVAR-'@PGMEM',VALUE-?P
    SNDMSG    MSG-?P,TO-XCTL.@ORGPROF
    CHGCMVAR '@OUTDSN',XSYSLIST
    RETURN    PGMEC-?PGMEC


```
