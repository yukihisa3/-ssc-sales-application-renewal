# PNJH9301

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/PNJH9301.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *   サカタのタネ　　　　　　　　　　　　　　　            *  ./
/. *   SYSTEM-NAME :    ＡＭＡＺＯＮ　ＥＤＩ                 *  ./
/. *   JOB-ID      :    PNJH9301                             *  ./
/. *   JOB-NAME    :    AMAZON受注データ変換                 *  ./
/. *   UPDATE      :    2020/11/10 S22460460                 *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?PGCHK)

/.##ﾊﾟﾗﾒﾀ定義##./
    PARA      ?HIDUKE   ,STRING*8,IN,VALUE-'        ' /.受信日付./
    PARA      ?JIKAN    ,STRING*4,IN,VALUE-'    '  /.受信時間./
    PARA      ?PGCHK    ,STRING*1,OUT,VALUE-' '    /.ｴﾗｰﾁｪｯｸ./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?TOKCD    ,STRING*8,VALUE-'        ' /.受信取引先./
    VAR       ?LINE     ,STRING*1,VALUE-' '        /.回線./
    VAR       ?YUSEN    ,STRING*1,VALUE-' '        /.回線優先./
    VAR       ?LIBNM    ,STRING*8,VALUE-'        ' /.集信LIB./
    VAR       ?FILNM    ,STRING*8,VALUE-'        ' /.集信FILE./
    VAR       ?JKEKA    ,STRING*4,VALUE-'    '     /.結果./
    VAR       ?PGMEC    ,INTEGER                   /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                 /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                 /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)              /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                 /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PNJH9301' /.PROGRAM-ID./
    VAR       ?STEP     ,STRING*8                  /.STEP-ID./
    VAR       ?HTIME    ,STRING*8,VALUE-'        ' /.ｼｽﾃﾑ時間退避用./
    VAR       ?HTIMES   ,STRING*4,VALUE-'    '     /.変換開始時間./
    VAR       ?HTIMEE   ,STRING*4,VALUE-'    '     /.変換終了時間./
    VAR       ?TIMES    ,STRING*4,VALUE-'0000'     /.受信開始./
    VAR       ?TIMEE    ,STRING*4,VALUE-'0000'     /.受信終了./
    VAR       ?SYORINM1 ,STRING*40
    VAR       ?SYORINM2 ,STRING*50
    VAR       ?SYORINM3 ,STRING*50
    VAR       ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN5 ,STRING*7,VALUE-'0000000'
    VAR       ?ERR      ,STRING*2,VALUE-'  '
    VAR       ?JOB      ,STRING*1,VALUE-'2'      /.2:受注ﾃﾞｰﾀ取込./
/.##ﾃﾞｰﾀ変換PG用ﾊﾟﾗﾒﾀ##./
    VAR       ?PARA     ,STRING*14,VALUE-'              '
/.##結果FLG用##./
    VAR       ?KEKA     ,STRING*4,VALUE-'    '     /.結果FLGﾊﾟﾗﾒﾀ./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR       ?LIBN     ,NAME                      /.ﾗｲﾌﾞﾗﾘ名前型./
    VAR       ?FILN     ,NAME                      /.ﾌｧｲﾙ名前型./
    VAR       ?FILLIB   ,NAME!MOD                  /.ﾌｧｲﾙ拡張用./
    VAR       ?FILID    ,STRING*17                 /.ﾌｧｲﾙ名表示用./

    VAR       ?SDTYMD   ,STRING*8,VALUE-'00000000' /. 開始作成日 ./
    VAR       ?EDTYMD   ,STRING*8,VALUE-'99999999' /. 終了作成日 ./
    VAR       ?SYTANCD  ,STRING*2,VALUE-'  '      /. 処理担当者 ./
    VAR       ?KBN      ,STRING*1,VALUE-'1'       /. ｵﾝﾗｲﾝ／手書 ./
    VAR       ?KANRINO  ,STRING*8,VALUE-'        ' /. 管理番号 ./
    VAR       ?BACHINO  ,STRING*20,VALUE-'                    '
                                                  /.バッチＮｏ./
    VAR       ?BUMON    ,STRING*4,VALUE-'    '    /.部門名./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '      /.担当者CD./
    VAR       ?KEKKA    ,STRING*2,VALUE-'  '      /.結果./

/.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
    SNDMSG MSG-'＊　データ変換開始　　＊',TO-XCTL
    SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL

    DEFLIBL TOKELIB/TOKDLIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB/TOKDTLIB

/.## ログインユーザー情報取得 ##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    ?PGMEC    :=   @PGMEC
    IF        ?PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.  データ変換                                                  ./
NCV9301B:

    ?STEP :=   'NCV9301B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    CALL      PGM-NCV9301B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,?LINE,
                                   ?YUSEN,?LIBNM,?FILNM,?JKEKA)
    ?PGMEC    :=   @PGMEC
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##ﾃﾞｰﾀ変換PGﾍのﾊﾟﾗﾒﾀ作成##./
    ?PARA :=   ?HIDUKE && ?JIKAN && ?LINE && ?YUSEN
    SNDMSG ?PARA,TO-XCTL.@ORGPROF,JLOG-@YES

/.##受信ﾌｧｲﾙの編集##./
    ?FILN   :=  %NAME(?FILNM)
    ?LIBN   :=  %NAME(?LIBNM)
    ?FILLIB :=  %NCAT(?FILN,?LIBN)
    ?FILID  :=  %STRING(?FILLIB)
    SNDMSG ?FILID,TO-XCTL.@ORGPROF,JLOG-@YES

   /.##変換開始時間取得##./
    ?HTIME  := @STIME
    ?HTIMES := %SBSTR(?HTIME,1,4)

    CALL SCVMSG.TOKELIB,PARA-('ｼﾞｭｼﾝDT ﾍﾝｶﾝ   START')

/.  AMAZON取込データ編集(受注)                                  ./
NJH9300B:

    ?STEP :=   'NJH9300B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-AMZJYUW1,TOFILE-?FILLIB
    CALL      PGM-NJH9300B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,
                                          ?BUMON,?TANCD,
                                          ?TOKCD,?SYORIKN1,?ERR)
    ?PGMEC    :=   @PGMEC
    SNDMSG    ?HIDUKE,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    ?JIKAN,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    ?BUMON,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    ?TANCD,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    ?TOKCD,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    ?SYORIKN1,TO-XCTL.@ORGPROF,JLOG-@YES
    SNDMSG    ?ERR,TO-XCTL.@ORGPROF,JLOG-@YES
    IF        ?PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO DATESET
    ELSE
         IF   ?ERR       =  'OK'  THEN
              /.##正常終了ｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K522'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
         ELSE
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO DATESET
         END
    END

NJH9310B:

    ?STEP :=   'NJH9310B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-AMZJYUL1,TOFILE-AMZJYUL1.TOKDTLIB
    OVRF      FILE-AMZJYUL2,TOFILE-AMZJYUL2.TOKDTLIB
    OVRF      FILE-JHSHENL1,TOFILE-JHSHENL1.TOKFLIB
    OVRF      FILE-AMZJOHL1,TOFILE-AMZJOHL1.TOKDTLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    OVRF      FILE-JSMKENL1,TOFILE-JSMKENL1.TOKJLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRVLDF   FILE-VLD500,TOFILE-LD500.XUCL

    CALL      PGM-NJH9310B.TOKSOLIB,PARA-(?PARA)
    ?PGMEC    :=   @PGMEC
    IF        ?PGMEC    ^=   0    THEN
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

  /.取込結果リスト ./
    DEFLIBL TOKELIBO/TOKELIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    ?SYORINM1 := '【ＡＭＡＺＯＮ受注データ変換・伝票データ作成】'
    ?SYORINM2 := 'ＡＭＡＺＯＮ受注データ取込　終了しました。'
    ?SYORINM3 := '件数１：受注件数'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)

    DEFLIBL TOKELIB/TOKDLIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB/TOKDTLIB

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

    OVRF    FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    CALL    PGM-NCV0170B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD,
                                       ?TIMES,?TIMEE,?HTIMES,?HTIMEE)
    ?PGMEC := @PGMEC
    IF   ?PGMEC ^= 0 THEN
         ?KEKA := 'K519'
         GOTO      ABEND
    END

    /. 0件の場合はCSV出力しない./
    IF        ?SYORIKN1 = '0000000' THEN
              GOTO    ABEND
    END

/.##AMAZON出荷確認CSV出力##  ./
    CALL SCVMSG.TOKELIB,PARA-('ﾍﾝｶﾝ CSV OUTPUT     ')
SSY9305V:

    ?STEP :=   'SSY9305V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-AMZCHKW1,TOFILE-AMZJYUL3.TOKDTLIB
    CALL      SSY9305V.TOKSOLIB,PARA-(?JOB)
    ?PGMEC := @PGMEC
    IF        ?PGMEC  ^=  0  THEN
              ?KEKA   :=    'K519'
              GOTO    ABEND
    END

/.##ＣＳＶ転送##./
    CALL SCVMSG.TOKELIB,PARA-('ﾍﾝｶﾝ CSV ﾃﾝｿｳ       ')
FIMPORT1:

    ?STEP :=   'FIMPORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-AMZCHKCS.TOKDTLIB,
            TYPE-@FILE,
            PARA-AMAZON,
            UNIT-2,
            OPR-@NO
    ?PGMEC := @PGMEC
    IF        ?PGMEC  ^=  0  THEN
              ?KEKA   :=    'K519'
              GOTO    ABEND
    END

/.##件数リスト出力##./
    CALL SCVMSG.TOKELIB,PARA-('ｹﾝｽｳ LST       START')
KENLSTAZ:

    ?STEP :=   'KENLSTAZ'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SBMJOB JOB-KENLSTAZ,JOBD-CVCS.XUCL,JOBK-@B,PGM-KENLST.TOKCLIBO,
           VSIZE-5,RSIZE-16,LOG-@YES!1024,PARA-(?HIDUKE,?JIKAN,
           ?TOKCD,?LINE,?YUSEN)


/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    IF     ?ERR   =   'ER'   THEN
           GOTO   ABEND
    END

    CALL SCVMSG.TOKELIB,PARA-('ｾｲｼﾞｮｳ ｼｭｳﾘｮｳ SJH310')

   /.##ｴﾗｰﾘｽﾄ発行##./
    SBMJOB JOB-KEKALIST,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
           PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?PGCHK := '0'
    RETURN    PGMEC-?PGMEC

/.##異常終了時##./
ABEND:

    CALL SCVMSG.TOKELIB,PARA-('ｲｼﾞｮｳ ｼｭｳﾘｮｳ  SJH310')

   /.##ｴﾗｰ結果更新##./
    DEFLIBL TOKELIB/TOKDLIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB/TOKDTLIB
    CALL PGM-NCV0090B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
   /.##ｴﾗｰﾘｽﾄ発行##./
    SBMJOB JOB-KEKALIST,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
           PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
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

    IF     ?ERR   =   'ER'   THEN
           ?PGCHK :=  '2'
    ELSE
           ?PGCHK :=  '1'
    END

    ?KEKA := ?JKEKA

    RETURN    PGMEC-@PGMEC

```
