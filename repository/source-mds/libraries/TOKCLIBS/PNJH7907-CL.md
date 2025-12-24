# PNJH7907

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PNJH7907.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    変換伝票データ作成                   *  ./
/. *   JOB-ID      :    PNJH7907                             *  ./
/. *   JOB-NAME    :    ダイキ資材　九州サンコー             *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?PGCHK)

/.##ﾊﾟﾗﾒﾀ定義##./
    PARA      ?HIDUKE   ,STRING*8,IN,VALUE-'        ' /.受信日付./
    PARA      ?JIKAN    ,STRING*4,IN,VALUE-'    '     /.受信時間./
    PARA      ?PGCHK    ,STRING*1,OUT,VALUE-' '           /.ｴﾗｰﾁｪｯｸ./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?TOKCD    ,STRING*8,VALUE-'        ' /.受信取引先./
    VAR       ?LINE     ,STRING*1,VALUE-' '        /.回線./
    VAR       ?YUSEN    ,STRING*1,VALUE-' '        /.回線優先./
    VAR       ?LIBNM    ,STRING*8,VALUE-'        ' /.集信LIB./
    VAR       ?FILNM    ,STRING*8,VALUE-'        ' /.集信FILE./
    VAR       ?JKEKA    ,STRING*4,VALUE-'    '    /.結果./
    VAR       ?TOKCD1   ,STRING*8,VALUE-'        ' /.取引先ＣＤ1./
    VAR       ?TOKCD2   ,STRING*8,VALUE-'        ' /.取引先ＣＤ2./
    VAR       ?TOKCK1   ,STRING*1,VALUE-' '        /.取引先ﾁｪｯｸ1./
    VAR       ?TOKCK2   ,STRING*1,VALUE-' '        /.取引先ﾁｪｯｸ2./
    VAR       ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PNJH7907'  /.PROGRAM-ID./
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

    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB/TOKDTLIB

/.  データ変換                                                  ./
NCV7920B:

    ?STEP :=   'NCV7920B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    CALL      PGM-NCV7920B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD,?LINE,
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

    CALL SCVMSG.TOKELIB,PARA-('ｼﾞｭｼﾝDT ﾍﾝｶﾝ   START')
/.  データ変換                                                  ./
NJH7907B:

    ?STEP :=   'NJH7907B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-CVCSG001,TOFILE-?FILLIB
    OVRF      FILE-JSMKENL1,TOFILE-JSMKENL1.TOKJLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JHMRUTL1,TOFILE-JHMRUTL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JHSHENL1,TOFILE-JHSHENL1.TOKFLIB
    OVRF      FILE-KMJOHOL1,TOFILE-KMJOHOL1.TOKFLIB
    OVRVLDF   FILE-VLD500,TOFILE-LD500.XUCL
    CALL      PGM-NJH7907B.TOKELIBO,
    PARA-(?HIDUKE,?JIKAN,?LINE,?YUSEN,?TOKCD1,?TOKCD2,?TOKCK1,
          ?TOKCK2)
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

    CALL SCVMSG.TOKELIB,PARA-('ｼﾞｭｼﾝDT ﾍﾝｶﾝ   END  ')
   /.##変換開始時間取得##./
    ?HTIME  := @STIME
    ?HTIMEE := %SBSTR(?HTIME,1,4)

/.##受信時間開始／終了、変換時間開始／終了更新##./
    CALL SCVMSG.TOKELIB,PARA-('ﾍﾝｶﾝ TIME SET       ')
DATESET:

    ?STEP :=   'DATESET '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF  ?TOKCK1 = '1'  THEN
        OVRF    FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
        CALL    PGM-NCV0170B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD1,
                                       ?TIMES,?TIMEE,?HTIMES,?HTIMEE)
    END
    IF  ?TOKCK2 = '1'  THEN
        OVRF    FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
        CALL    PGM-NCV0170B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD2,
                                       ?TIMES,?TIMEE,?HTIMES,?HTIMEE)
    END
    ?PGMEC := @PGMEC
    IF   ?PGMEC ^= 0 THEN
         ?KEKA := 'K519'
         GOTO      ABEND
    END


/.##振分け件数ﾘｽﾄ出力##./
    CALL SCVMSG.TOKELIB,PARA-('ｹﾝｽｳ LST       START')
KENLST11:

    ?STEP :=   'KENLST11'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '*** TOKCK1 = ' && ?TOKCK1 && ' TOKCD1 = ' && ?TOKCD1
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*** TOKCK2 = ' && ?TOKCK2 && ' TOKCD2 = ' && ?TOKCD2
    SNDMSG    ?MSGX,TO-XCTL
    IF  ?TOKCK1 = '1'  THEN
        SBMJOB JOB-KENLSTA1,JOBD-CVCS.XUCL,JOBK-@B,PGM-KENLST.TOKCLIBO,
        VSIZE-5,RSIZE-16,LOG-@YES!1024,PARA-(?HIDUKE,?JIKAN,
        ?TOKCD1,?LINE,?YUSEN)
    END
    IF  ?TOKCK2 = '1'  THEN
        SBMJOB JOB-KENLSTB1,JOBD-CVCS.XUCL,JOBK-@B,PGM-KENLST.TOKCLIBO,
        VSIZE-5,RSIZE-16,LOG-@YES!1024,PARA-(?HIDUKE,?JIKAN,
        ?TOKCD2,?LINE,?YUSEN)
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    CALL SCVMSG.TOKELIB,PARA-('ｾｲｼﾞｮｳ ｼｭｳﾘｮｳ SJH310')

   /.##ｴﾗｰﾘｽﾄ発行##./
    IF  ?TOKCK1 = '1'  THEN
        SBMJOB JOB-KEKALTA1,JOBD-CVCS.XUCL,JOBK-@B,
               PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
               @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
               PARA-(?HIDUKE,?JIKAN,?TOKCD1,?KEKA)
    END
    IF  ?TOKCK2 = '1'  THEN
        SBMJOB JOB-KEKALTB1,JOBD-CVCS.XUCL,JOBK-@B,
               PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
               @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
               PARA-(?HIDUKE,?JIKAN,?TOKCD2,?KEKA)
    END
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?PGCHK := '0'
    RETURN    PGMEC-@PGMEC

/.##異常終了時##./
ABEND:

    CALL SCVMSG.TOKELIB,PARA-('ｲｼﾞｮｳ ｼｭｳﾘｮｳ  SJH310')

    IF  ?TOKCK1 = '1'  THEN
     /.##ｴﾗｰ結果更新##./
      DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB
      CALL PGM-NCV0090B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD1,?KEKA)
     /.##ｴﾗｰﾘｽﾄ発行##./
      SBMJOB JOB-KEKALSA1,JOBD-CVCS.XUCL,JOBK-@B,
             PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
             @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
             PARA-(?HIDUKE,?JIKAN,?TOKCD1,?KEKA)
    END
    IF  ?TOKCK2 = '1'  THEN
     /.##ｴﾗｰ結果更新##./
      DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB
      CALL PGM-NCV0090B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD2,?KEKA)
     /.##ｴﾗｰﾘｽﾄ発行##./
      SBMJOB JOB-KEKALSB1,JOBD-CVCS.XUCL,JOBK-@B,
             PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
             @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
             PARA-(?HIDUKE,?JIKAN,?TOKCD2,?KEKA)
    END
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
    ?PGCHK := '1'
/.  ?KEKA := ?JKEKA ./

    RETURN    PGMEC-@PGMEC

```
