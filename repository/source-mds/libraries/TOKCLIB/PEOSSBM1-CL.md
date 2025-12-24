# PEOSSBM1

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PEOSSBM1.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信管理システム                   *  ./
/. *   JOB-ID      :    PEOSSBMJ                             *  ./
/. *   JOB-NAME    :    共通Ｋ６５００側受信起動             *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TOKCD,P4-?PASS,P5-?HENID,
         P6-?LINE,P7-?YUSEN,P8-?LIBNM,P9-?FILNM,P10-?KJOB,P11-?JRL,
         P12-?JYUKBN)
/.##ﾊﾟﾗﾒﾀ定義##./
    PARA      ?HIDUKE   ,STRING*8,IN,VALUE-'        ' /.受信日付./
    PARA      ?JIKAN    ,STRING*4,IN,VALUE-'    '     /.受信時間./
    PARA      ?TOKCD    ,STRING*8,IN,VALUE-'        ' /.受信取引先./
    PARA      ?PASS     ,STRING*13,IN,VALUE-'             '
    PARA      ?HENID    ,STRING*8,IN,VALUE-'        ' /.変換ID./
    PARA      ?LINE     ,STRING*1,IN,VALUE-' '        /.回線./
    PARA      ?YUSEN    ,STRING*1,IN,VALUE-' '        /.回線優先./
    PARA      ?LIBNM    ,STRING*8,IN,VALUE-'        ' /.集信LIB./
    PARA      ?FILNM    ,STRING*8,IN,VALUE-'        ' /.集信FILE./
    PARA      ?KJOB     ,STRING*8,IN,VALUE-'        ' /.起動JOB./
    PARA      ?JRL      ,STRING*4,IN,VALUE-'    '     /.ﾚｺｰﾄﾞ長./
    PARA      ?JYUKBN   ,STRING*1,IN,VALUE-' '        /.受信区分./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PSJH1101'  /.PROGRAM-ID./
    VAR       ?STEP     ,STRING*8                   /.STEP-ID./
/.##ｹｯｶFLG##./
    VAR       ?KEKA     ,STRING*2,VALUE-'  '        /.更新ｹｯｶFLG./

/.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.  SNDMSG    ?HIDUKE   ,TO-XCTL.@ORGPROF
    SNDMSG    ?JIKAN    ,TO-XCTL.@ORGPROF
    SNDMSG    ?TOKCD    ,TO-XCTL.@ORGPROF
    SNDMSG    ?PASS     ,TO-XCTL.@ORGPROF
    SNDMSG    ?HENID    ,TO-XCTL.@ORGPROF
    SNDMSG    ?LINE     ,TO-XCTL.@ORGPROF
    SNDMSG    ?YUSEN    ,TO-XCTL.@ORGPROF
    SNDMSG    ?LIBNM    ,TO-XCTL.@ORGPROF
    SNDMSG    ?FILNM    ,TO-XCTL.@ORGPROF
    SNDMSG    ?KJOB     ,TO-XCTL.@ORGPROF
    SNDMSG    ?JRL      ,TO-XCTL.@ORGPROF ./
/.##DEFLIBL##./
    DEFLIBL TOKELIB/TOKFLIB
/.##ﾊﾟﾗﾒﾀﾌｧｲﾙ作成##./
JOBSTR:

    ?STEP :=   'JOBSTR  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ＩＮＳ回線＋優先順位１の場合##./
    IF ?LINE  =  'I'  &  ?YUSEN  =  '1'
       THEN
    SNDMSG MSG-'## INS-1 ｼﾞｭｼﾝJOB START ##',TO-XCTL.@ORGPROF,JLOG-@YES
       /.##ﾊﾟﾗﾒﾀﾌｧｲﾙ作成##./
       OVRF FILE-PARAFILE,TOFILE-PARAINS1.TOKFLIB
       CALL PGM-SCV0100B.TOKELIB,
            PARA-(?HIDUKE,?JIKAN,?TOKCD,?PASS,?HENID,?LINE,?YUSEN,
                  ?LIBNM,?FILNM,?KJOB,?JRL,?JYUKBN)
       IF   @PGMEC  ^=  0  THEN
            ?KEKA  :=  '80'
            OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
          CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
            GOTO  ABEND
       END
       /.##ﾊﾟﾗﾒﾀﾌｧｲﾙｿｳｼﾝ##./
     SNDMSG MSG-'## ﾊﾟﾗﾒﾀﾌｧｲﾙ ｿｳｼﾝ START ##',TO-XCTL.@ORGPROF,JLOG-@YES
/.     SNDFILE COM-KGPATH01.KGNODE01,SFILE-PARAINS1.TOKFLIB,
               DFILE-'PARAINS1',RL-150,BF-1 ./
       IF   @PGMEC  ^=  0  THEN
            ?KEKA  :=  '81'
            OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
        CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
            GOTO  ABEND
       END
       /.##K6500側JOB起動##./
     SNDMSG MSG-'## K6500 JOB      START ##',TO-XCTL.@ORGPROF,JLOG-@YES
/.## ﾃｽﾄ運用定義 ##./
       CALL PGM-CVCSINS1.TOKELIB
   /.  SNDRSJJ COM-KGPATH01.KGNODE01,DFILE-'CVCSINS1'./
       IF   @PGMEC  ^=  0  THEN
            ?KEKA  :=  '82'
            OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
         CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
            GOTO  ABEND
       END
    END
/.##電話回線＋優先順位１の場合##./
    IF ?LINE  =  'T'  &  ?YUSEN  =  '1'
       THEN
    SNDMSG MSG-'## TEL-1 ｼﾞｭｼﾝJOB START ##',TO-XCTL.@ORGPROF,JLOG-@YES
       /.##ﾊﾟﾗﾒﾀﾌｧｲﾙ作成##./
       OVRF FILE-PARAFILE,TOFILE-PARATEL1.TOKFLIB
       CALL PGM-SCV0100B.TOKELIB,
            PARA-(?HIDUKE,?JIKAN,?TOKCD,?PASS,?HENID,?LINE,?YUSEN,
                  ?LIBNM,?FILNM,?KJOB,?JRL,?JYUKBN)
       IF   @PGMEC  ^=  0  THEN
            ?KEKA  :=  '80'
            OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
         CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
            GOTO  ABEND
       END
       /.##ﾊﾟﾗﾒﾀﾌｧｲﾙｿｳｼﾝ##./
     SNDMSG MSG-'## ﾊﾟﾗﾒﾀﾌｧｲﾙ ｿｳｼﾝ START ##',TO-XCTL.@ORGPROF,JLOG-@YES
     /.SNDFILE COM-KGPATH01.KGNODE01,SFILE-PARATEL1.TOKFLIB,
               DFILE-'PARATEL1',RL-150,BF-1                ./
       IF   @PGMEC  ^=  0  THEN
            ?KEKA  :=  '81'
            OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
         CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
            GOTO  ABEND
       END
       /.##K6500側JOB起動##./
     SNDMSG MSG-'## K6500 JOB      START ##',TO-XCTL.@ORGPROF,JLOG-@YES
    /.##ﾃｽﾄ運用##./
       CALL PGM-CVCSTEL1.TOKELIB
    /. SNDRSJJ COM-KGPATH01.KGNODE01,DFILE-'CVCSTEL1'./
       IF   @PGMEC  ^=  0  THEN
            ?KEKA  :=  '82'
            OVRF FILE-JHMTJSL1,TOFILE-JHMTJSL1.TOKFLIB
         CALL PGM-SCV0090B.TOKELIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
            GOTO  ABEND
       END
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RETURN    PGMEC-@PGMEC

/.##異常終了時##./
ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    SNDMSG ?PGMEM,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

```
