# PNJH4591

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PNJH4591.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    変換伝票データ作成                   *  ./
/. *   JOB-ID      :    PNJH4591                             *  ./
/. *   JOB-NAME    :    イオン新（西日本分）　　             *  ./
/. ***********************************************************  ./
    PGM  (P1-?KBN)
/.###ﾊﾟﾗﾒﾀ定義####./
    PARA ?KBN     ,STRING*2,OUT,VALUE-'  ' /.OK,NG./
/.##ﾊﾟﾗﾒﾀ定義##./
    VAR       ?HIDUKE   ,STRING*8,VALUE-'        ' /.受信日付./
    VAR       ?JIKAN    ,STRING*4,VALUE-'    '     /.受信時間./
    VAR       ?TOKCD    ,STRING*8,VALUE-'        ' /.受信取引先./
    VAR       ?LINE     ,STRING*1,VALUE-' '        /.回線./
    VAR       ?YUSEN    ,STRING*1,VALUE-' '        /.回線優先./
    VAR       ?LIBNM    ,STRING*8,VALUE-'        ' /.集信LIB./
    VAR       ?FILNM    ,STRING*8,VALUE-'        ' /.集信FILE./
    VAR       ?JKEKA    ,STRING*4,VALUE-'    '    /.結果./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PNJH4591'  /.PROGRAM-ID./
    VAR       ?STEP     ,STRING*8                   /.STEP-ID./
    VAR       ?HTIME    ,STRING*8,VALUE-'        ' /.ｼｽﾃﾑ時間退避用./
    VAR       ?HTIMES   ,STRING*4,VALUE-'    '      /.変換開始時間./
    VAR       ?HTIMEE   ,STRING*4,VALUE-'    '      /.変換終了時間./
    VAR       ?TIMES    ,STRING*4,VALUE-'0000'     /.受信開始./
    VAR       ?TIMEE    ,STRING*4,VALUE-'0000'     /.受信終了./
/.##ﾃﾞｰﾀ変換PG用ﾊﾟﾗﾒﾀ##./
    VAR       ?PARA     ,STRING*14,VALUE-'              '
/.##結果FLG用##./
    VAR       ?KEKA     ,STRING*4,VALUE-'    '      /.結果FLGﾊﾟﾗﾒﾀ./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR       ?LIBN     ,NAME                       /.ﾗｲﾌﾞﾗﾘ名前型./
    VAR       ?FILN     ,NAME                       /.ﾌｧｲﾙ名前型./
    VAR       ?FILLIB   ,NAME!MOD                   /.ﾌｧｲﾙ拡張用./
    VAR       ?FILID    ,STRING*17                  /.ﾌｧｲﾙ名表示用./

/.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
    SNDMSG MSG-'＊　データ変換開始　　＊',TO-XCTL
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL

    DEFLIBL TOKELIB/TOKFLIB/OSKELIB

/.  データ変換                                                  ./
SCV1001B:

    ?STEP :=   'NCV1001B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKFLIB
    CALL      PGM-NCV1002B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD,?LINE,
                                   ?YUSEN,?LIBNM,?FILNM,?JKEKA)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##ﾃﾞｰﾀ変換PGﾍのﾊﾟﾗﾒﾀ作成##./
    ?PARA :=   ?HIDUKE && ?JIKAN && ?LINE && ?YUSEN
    SNDMSG ?PARA,TO-XCTL.@ORGPROF,JLOG-@YES

/.##受信ﾌｧｲﾙノ編集##./
    ?FILN   :=  %NAME(?FILNM)
    ?LIBN   :=  %NAME(?LIBNM)
    ?FILLIB :=  %NCAT(?FILN,?LIBN)
    ?FILID  :=  %STRING(?FILLIB)
    SNDMSG ?FILID,TO-XCTL.@ORGPROF,JLOG-@YES

   /.##変換開始時間取得##./
    ?HTIME  := @STIME
    ?HTIMES := %SBSTR(?HTIME,1,4)


    CALL SCVMSG.TOKCLIBO,PARA-('ｼﾞｭｼﾝDT ﾍﾝｶﾝ   START')
/.  データ変換                                                  ./
NJH4591B:

    ?STEP :=   'NJH4591B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-CVCSG222,TOFILE-?FILLIB
    OVRF      FILE-JHMKENL1,TOFILE-JHMKENL1.TOKFLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JHMRUTL1,TOFILE-JHMRUTL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JHSHENL1,TOFILE-JHSHENL1.TOKFLIB
    OVRVLDF   FILE-VLD500,TOFILE-LD500.XUCL
    CALL      PGM-NJH4591B.TOKELIBO,PARA-(?PARA)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    ELSE
              /.##正常終了ｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K522'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    END

    CALL SCVMSG.TOKCLIBO,PARA-('ｼﾞｭｼﾝDT ﾍﾝｶﾝ   END  ')
   /.##変換開始時間取得##./
    ?HTIME  := @STIME
    ?HTIMEE := %SBSTR(?HTIME,1,4)

/.##受信時間開始／終了、変換時間開始／終了更新##./
    CALL SCVMSG.TOKCLIBO,PARA-('ﾍﾝｶﾝ TIME SET       ')
DATESET:

    ?STEP :=   'DATESET '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF    FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKFLIB
    CALL    PGM-NCV0170B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD,
                                       ?TIMES,?TIMEE,?HTIMES,?HTIMEE)
    ?PGMEC := @PGMEC
    IF   ?PGMEC ^= 0 THEN
         ?KEKA := 'K519'
         GOTO      ABEND
    END


/.##振分け件数ﾘｽﾄ出力##./
    CALL SCVMSG.TOKCLIBO,PARA-('ｹﾝｽｳ LST       START')
KENLST:

    ?STEP :=   'KENLST  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SBMJOB JOB-KENLST02,JOBD-CVCS.XUCL,JOBK-@B,PGM-KENLST.TOKCLIBO,
           VSIZE-5,RSIZE-16,LOG-@YES!1024,PARA-(?HIDUKE,?JIKAN,
           ?TOKCD,?LINE,?YUSEN)


/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    CALL SCVMSG.TOKCLIBO,PARA-('ｾｲｼﾞｮｳ ｼｭｳﾘｮｳ PNJH4591')

   /.##ｴﾗｰﾘｽﾄ発行##./
    SBMJOB JOB-KEKALS02,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
           PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?KBN  :=  'OK'
    RETURN    PGMEC-@PGMEC

/.##異常終了時##./
ABEND:

    CALL SCVMSG.TOKCLIBO,PARA-('ｲｼﾞｮｳ ｼｭｳﾘｮｳ  PNJH4591')

   /.##ｴﾗｰ結果更新##./
    DEFLIBL TOKELIB/TOKFLIB
    CALL PGM-NCV0090B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
   /.##ｴﾗｰﾘｽﾄ発行##./
    SBMJOB JOB-KEKALS02,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
           PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
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
    ?KEKA := ?JKEKA
    ?KBN  :=  'NG'

    RETURN    PGMEC-@PGMEC

```
