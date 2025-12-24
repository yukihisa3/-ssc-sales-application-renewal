/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    変換伝票データ作成                   *  ./
/. *   JOB-ID      :    PNJH7531                             *  ./
/. *   JOB-NAME    :    ＤＣＭサンワ（支払：資材部門）       *  ./
/. ***********************************************************  ./
    PGM

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
    VAR       ?PGMID    ,STRING*8,VALUE-'PNJH7531'  /.PROGRAM-ID./
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
/.-----------------------------------------------------------./
    VAR       ?CLID   ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1   ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'ＤＣＭサンワ　受領取込（資材）'

/.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##確認画面##./
STEP010:

    ?OPR1  :=  '＜ＤＣＭサンワ　支払データ取込＞'
    ?OPR2  :=  '取引先：資材部門（３９２４、３９２４１）'
    ?OPR3  :=  'ＳＮＳシステムよりデータダウンロードし、'
    ?OPR4  :=  'バックアップ処理が完了しておりますか？'
    ?OPR5  :=  '確認後、実行して下さい。'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/TOKDLIB

/.##前回分の受信データを削除##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE ONLNSNSZ.ONLBLIB
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '前回分削除'
              GOTO ABEND
    END

/.##受信データ取込##./
PFEXPORT:

    ?STEP :=   'PFEXPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    FEXPORT FILE-ONLNSNSZ.ONLBLIB,MODE-@ADD,PARA-ONLSNS,
            UNIT-6
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '受信データ取込'
              GOTO ABEND
    END

/.##データ変換##./
NCV7531B:

    ?STEP :=   'NCV7531B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    CALL      PGM-NCV7531B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD,?LINE,
                                   ?YUSEN,?LIBNM,?FILNM,?JKEKA)
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '受信基本セット'
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

    CALL SCVMSG.TOKELIB,PARA-('ｾｲｷﾞｮﾊﾞｲﾄ DELE START')
/.##制御バイト削除##./
SSY7599B:

    ?STEP :=   'SSY7599B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-ONLSNSDT,TOFILE-?FILLIB
    OVRF      FILE-ONLSNSWK,TOFILE-ONLSNSWK.ONLBLIB
    CALL      PGM-SSY7599B.TOKELIBO
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '日本語制御バイト削除'
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?MSGX :=  '##EDIC制御ﾊﾞｲﾄ削除処理異常]]##'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
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

    CALL SCVMSG.TOKELIB,PARA-('ｼﾞｭｼﾝDT ﾍﾝｶﾝ   START')
/.##データ変換##./
NJH7531B:

    ?STEP :=   'NJH7531B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-CVCSG001,TOFILE-ONLSNSWK.ONLBLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-SWSIHAL1,TOFILE-SWSIHAL1.TOKDLIB
    CALL      PGM-NJH7531B.TOKELIBO,PARA-(?PARA)
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              ?KEKA4 := '受信データ変換（発注：資材）'
              GOTO ABEND
    ELSE
              /.##正常終了ｾｯﾄ##./
              ?KEKA := 'K523'
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

    OVRF    FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    CALL    PGM-NCV0170B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD,
                                       ?TIMES,?TIMEE,?HTIMES,?HTIMEE)
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA := 'K519'
              ?KEKA4 := '受信情報セット'
              GOTO      ABEND
    END

/.## 受信データ初期化 ##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-ONLNSNCL.ONLBLIB,PARA-ONLSNS,UNIT-9
    ?PGMEC := @PGMEC
    ?PGMEM := @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA := 'K519'
              ?KEKA4 := '受信ＤＴ初期化（サンワ資材）'
              GOTO      ABEND
    END


/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    CALL SCVMSG.TOKELIB,PARA-('ｾｲｼﾞｮｳ ｼｭｳﾘｮｳ SJH310')

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RETURN    PGMEC-@PGMEC

/.##異常終了時##./
ABEND:

    CALL SCVMSG.TOKELIB,PARA-('ｲｼﾞｮｳ ｼｭｳﾘｮｳ  SJH310')

   /.##ｴﾗｰ結果更新##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB
    CALL PGM-NCV0090B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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

    RETURN    PGMEC-@PGMEC
