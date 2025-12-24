# PDCM0020

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/PDCM0020.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　ＮＡＶＳ基幹（自動実行用）　　        *  ./
/. *   SYSTEM-NAME :    ＤＣＭ用発注データ変換処理（連続）　 *  ./
/. *   JOB-ID      :    PDCM0020                             *  ./
/. *   JOB-NAME    :    ＤＣＭ用発注データ変換処理（連続）   *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?JTOKCD,P4-?JIKKO,P5-?KTDATE,
       /.P6-?KTTIME,P7-?TANTO,P8-?SYORI,P9-?CLKEKA)./
         P6-?KTTIME,P7-?TANTO,P8-?SYORI)

/.##ﾊﾟﾗﾒﾀ定義##./
    PARA   ?HIDUKE ,STRING*8,IN,VALUE-'        '    /.受信日付./
    PARA   ?JIKAN  ,STRING*4,IN,VALUE-'    '        /.受信時間./
    PARA   ?JTOKCD ,STRING*8,IN,VALUE-'        '    /.受信取引先./
    PARA   ?JIKKO  ,STRING*10,IN,VALUE-'          ' /.実行番号./
    PARA   ?KTDATE ,STRING*8,IN,VALUE-'        '    /.起動日付./
    PARA   ?KTTIME ,STRING*6,IN,VALUE-'      '      /.起動時刻./
    PARA   ?TANTO  ,STRING*2,IN,VALUE-'  '          /.起動担当者./
    PARA   ?SYORI  ,STRING*10,IN,VALUE-'          ' /.処理番号./
/.  PARA   ?CLKEKA ,STRING*4,OUT,VALUE-'    './     /.結果./

    VAR       ?PGCHK    ,STRING*1,VALUE-' '           /.ｴﾗｰﾁｪｯｸ./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?TOKCD    ,STRING*8,VALUE-'        ' /.受信取引先./
    VAR       ?LINE     ,STRING*1,VALUE-' '        /.回線./
    VAR       ?YUSEN    ,STRING*1,VALUE-' '        /.回線優先./
    VAR       ?LIBNM    ,STRING*8,VALUE-'        ' /.集信LIB./
    VAR       ?FILNM    ,STRING*8,VALUE-'        ' /.集信FILE./
    VAR       ?JKEKA    ,STRING*4,VALUE-'    '    /.結果./
    VAR       ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECN ,STRING*8
    VAR       ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PDCM0020'  /.PROGRAM-ID./
    VAR       ?STEP     ,STRING*8                   /.STEP-ID./
    VAR       ?HTIME    ,STRING*8,VALUE-'        ' /.ｼｽﾃﾑ時間退避用./
    VAR       ?HTIMES   ,STRING*4,VALUE-'    '      /.変換開始時間./
    VAR       ?HTIMEE   ,STRING*4,VALUE-'    '      /.変換終了時間./
    VAR       ?TIMES    ,STRING*4,VALUE-'0000'     /.受信開始./
    VAR       ?TIMEE    ,STRING*4,VALUE-'0000'     /.受信終了./
    VAR       ?HTOKCD   ,STRING*7,VALUE-'       '   /.JOBD用./
/.##ﾃﾞｰﾀ変換PG用ﾊﾟﾗﾒﾀ##./
    VAR       ?PARA     ,STRING*22,VALUE-'                      '
    VAR       ?PARA1    ,STRING*22,VALUE-'                      '
/.##結果FLG用##./
    VAR       ?KEKA     ,STRING*4,VALUE-'    '      /.結果FLGﾊﾟﾗﾒﾀ./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR       ?LIBN     ,NAME                       /.ﾗｲﾌﾞﾗﾘ名前型./
    VAR       ?FILN     ,NAME                       /.ﾌｧｲﾙ名前型./
    VAR       ?FILLIB   ,NAME!MOD                   /.ﾌｧｲﾙ拡張用./
    VAR       ?FILID    ,STRING*17                  /.ﾌｧｲﾙ名表示用./
    VAR       ?TOKNM    ,STRING*16                  /.取引先名./
/.##JOB名称##./
    VAR       ?JOBNML   ,STRING*1,VALUE-'L'        /.JOB名LST./
    VAR       ?JOBNMK   ,STRING*1,VALUE-'K'        /.JOB名KEKA./
    VAR       ?JOBNM1   ,STRING*8,VALUE-'        ' /.JOB名1./
    VAR       ?JOBNAME1 ,NAME                      /.ﾌｧｲﾙ名前型./
    VAR       ?JOBNM2   ,STRING*8,VALUE-'        ' /.JOB名2./
    VAR       ?JOBNAME2 ,NAME                      /.ﾌｧｲﾙ名前型./
    /.更新区分./
    VAR ?KBN      ,STRING*01,VALUE-' '
    /.WKSTN名./
    VAR ?WKSTN    ,NAME
    VAR ?WSID     ,STRING*08,VALUE-'        '
    /.子実行ＮＯ./
    VAR ?KJIKKO   ,STRING*10,VALUE-'0000000000'
    /.処理名称./
    VAR ?SYORINM  ,STRING*42,
        VALUE-'　　　　　　　　　　　　　　　　　　　　'
    /.実行名称./
    VAR ?JIKKONM  ,STRING*42,
        VALUE-'　　　　　　　　　　　　　　　　　　　　'
    /.処理結果区分./
    VAR ?KEKKA    ,STRING*01,VALUE-' '
    /.終了ステータス./
    VAR ?STATUS   ,STRING*04,VALUE-'    '
    VAR ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                  /.      2    ./
    VAR ?KEKA2    ,STRING*40                  /.      3    ./
    VAR ?KEKA3    ,STRING*40                  /.      4    ./
    VAR ?KEKA4    ,STRING*40                  /.      5    ./
    /.区分./
    VAR ?ERKBN    ,STRING*03,VALUE-'   '
    /.バッチ日付./
    VAR ?ERDATE   ,STRING*08,VALUE-'00000000'
    /.バッチ時刻./
    VAR ?ERTIME   ,STRING*04,VALUE-'0000'
    /.バッチ取引先./
    VAR ?ERTKCD   ,STRING*08,VALUE-'00000000'

/.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?TOKCD     :=  ?JTOKCD
    ?MSGX :=  '実行ﾊﾞｯﾁNO = ' && ?HIDUKE && '-' && ?JIKAN && '-' &&
              ?TOKCD
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?PARA1 :=  ?HIDUKE && '-' && ?JIKAN && '-' && ?TOKCD

    CASE  ?JTOKCD OF
          # '00000880' #
          ?PGNM := 'ＤＣＭ取引先別変換（０００００８８０）'
          # '00000882' #
          ?PGNM := 'ＤＣＭ取引先別変換（０００００８８２）'
          # '00000883' #
          ?PGNM := 'ＤＣＭ取引先別変換（０００００８８３）'
          # '00001427' #
          ?PGNM := 'ＤＣＭ取引先別変換（００００１４２７）'
          # '00014282' #
          ?PGNM := 'ＤＣＭ取引先別変換（０００１４２７２）'
          # '00014273' #
          ?PGNM := 'ＤＣＭ取引先別変換（０００１４２７３）'
          # '00001731' #
          ?PGNM := 'ＤＣＭ取引先別変換（００００１７３１）'
          # '00001732' #
          ?PGNM := 'ＤＣＭ取引先別変換（００００１７３２）'
          # '00007601' #
          ?PGNM := 'ＤＣＭ取引先別変換（００００７６０１）'
          # '00007602' #
          ?PGNM := 'ＤＣＭ取引先別変換（００００７６０２）'
          # '00013938' #
          ?PGNM := 'ＤＣＭ取引先別変換（０００１３９３８）'
          # '00017137' #
          ?PGNM := 'ＤＣＭ取引先別変換（０００１７１３７）'
          # '00139381' #
          ?PGNM := 'ＤＣＭ取引先別変換（００１３９３８１）'
          # '00171371' #
          ?PGNM := 'ＤＣＭ取引先別変換（００１７１３７１）'
          # '00100403' #
          ?PGNM := 'ＤＣＭ取引先別変換（００１００４０３）'
          # '00100427' #
          ?PGNM := 'ＤＣＭ取引先別変換（００１００４２７）'
          # '00100441' #
          ?PGNM := 'ＤＣＭ取引先別変換（００１００４４１）'
          # '00100404' #
          ?PGNM := 'ＤＣＭ取引先別変換（００１００４０４）'
          # '00100428' #
          ?PGNM := 'ＤＣＭ取引先別変換（００１００４２８）'
          # '00100442' #
          ?PGNM := 'ＤＣＭ取引先別変換（００１００４４２）'
    ELSE        /.異常受信./
          ?PGNM := 'ＤＣＭ取引先別変換（９９９９９９９９）'
    END

/.##処理開始メッセージ##./
    ?MSGX :=  '##(' && ?TOKCD && ')発注ﾃﾞｰﾀ変換開始'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '##(' && ?PGMID && ')変換取引先:' && ?TOKCD
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB/TOKSOLIB/TOKDTLIB

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WSID    :=  %STRING(?WKSTN)
    ?MSGX    :=  '## WKSTN名 = ' && ?WSID  && 'TOKCD = ' && ?JTOKCD
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##自動受信時間監視／受信処理実行################################./
JOBNMHEN:

    ?STEP :=   'JOBNMHEN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    ?HTOKCD    :=  %SBSTR(?TOKCD,2,7)
    ?JOBNM1    :=  ?JOBNML && ?HTOKCD
    ?JOBNAME1  :=  %NAME(?JOBNM1)
    ?MSGX   :=  '## 件数LST JOB名 : ' && ?JOBNM1
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?JOBNM2    :=  ?JOBNMK && ?HTOKCD
    ?JOBNAME2  :=  %NAME(?JOBNM2)
    ?MSGX   :=  '## 件数ERR JOB名 : ' && ?JOBNM2
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES


/.##自動受信時間監視／受信処理実行################################./
NSY1000B:

    ?STEP :=   'NSY1000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '# PDCM0020 ' && ?PARA1 && ' SYORINO=1 #'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    /.##パラメタセット##./
    ?KBN     :=  '2'
    ?KJIKKO  :=  '0000000001'
    ?JIKKONM :=  '受信時間開始／受信処理実行制御　　　　　'
    ?KEKKA   :=  ' '
    ?STATUS  :=  '    '

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KTDATE,?KTTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END

    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    CALL      PGM-NSY1000B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,
                     ?TOKCD,?LINE,?YUSEN,?LIBNM,?FILNM,?JKEKA)
    ?PGMEC    :=    @PGMEC
    ?PGMECX   :=    %STRING(@PGMEC)
    ?PGMECN   :=    %SBSTR(?PGMECX,8,4)
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            /.##パラメタセット##./
            ?KBN     :=  '4'
            ?KEKKA   :=  '2'
            ?STATUS  :=  ?PGMECN
    ELSE
            /.##パラメタセット##./
            ?KBN     :=  '3'
            ?KEKKA   :=  '1'
            ?STATUS  :=  '    '
    END

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KTDATE,?KTTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0      THEN
              GOTO ABEND
    END

    IF        ?PGMECN   ^=  '0000'  THEN
              GOTO ABEND
    END

/.##データ変換ＰＧ用パラメタ作成##################################./
PARAHENS:

    ?STEP :=   'PARAHENS'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '# PDCM0020 ' && ?PARA1 && ' SYORINO=2 #'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    ?PARA :=   ?HIDUKE && ?JIKAN && ?TOKCD && ?LINE && ?YUSEN
    ?MSGX :=  '#ﾃﾞｰﾀ変換ﾊﾟﾗﾒﾀ = ' && ?PARA
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##受信ファイル名編集############################################./
FILEHENS:

    ?STEP :=   'FILEHENS'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '# PDCM0020 ' && ?PARA1 && ' SYORINO=3 #'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    ?FILN   :=  %NAME(?FILNM)
    ?LIBN   :=  %NAME(?LIBNM)
    ?FILLIB :=  %NCAT(?FILN,?LIBN)
    ?FILID  :=  %STRING(?FILLIB)
    ?MSGX   :=  '## 受信F名 :' && ?FILID
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##変換開始時刻取得#############################################./
HENTIMES:

    ?STEP :=   'HENTIMES'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '# PDCM0020 ' && ?PARA1 && ' SYORINO=4 #'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    ?HTIME  := @STIME
    ?HTIMES := %SBSTR(?HTIME,1,4)

    CALL SCVMSG.TOKELIB,PARA-('ｼﾞｭｼﾝDT ﾍﾝｶﾝ   START')
/.##発注データ変換処理##########################################./
NSY0040B:

    ?STEP :=   'NSY0040B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '# PDCM0020 ' && ?PARA1 && ' SYORINO=5 #'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    /.##パラメタセット##./
    ?KBN     :=  '2'
    ?KJIKKO  :=  '0000000002'
    ?JIKKONM :=  '発注データ変換処理　　　　　　　　　　　'
    ?KEKKA   :=  ' '
    ?STATUS  :=  '    '

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KTDATE,?KTTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END

    OVRF      FILE-DNJOHOL1,TOFILE-DNJOHOL1.TOKDTLIB
    OVRF      FILE-JSMKENL1,TOFILE-JSMKENL1.TOKJLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JHMRUTL1,TOFILE-JHMRUTL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JHSHENL1,TOFILE-JHSHENL1.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRVLDF   FILE-VLD500,TOFILE-LD500.XUCL
    OVRF      FILE-DCMHSBL1,TOFILE-DCMHSBL1.TOKDTLIB
    CALL      PGM-NSY0040B.TOKSOLIB,PARA-(?PARA)
    ?PGMEC    :=    @PGMEC
    ?PGMECX   :=    %STRING(@PGMEC)
    ?PGMECN   :=    %SBSTR(?PGMECX,8,4)
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            /.##パラメタセット##./
            ?KBN     :=  '4'
            ?KEKKA   :=  '2'
            ?STATUS  :=  ?PGMECN
    ELSE
            /.##パラメタセット##./
            ?KBN     :=  '3'
            ?KEKKA   :=  '1'
            ?STATUS  :=  '    '
    END

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KTDATE,?KTTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0      THEN
              GOTO ABEND
    END

    IF        ?PGMECN   ^=  '0000'  THEN    /.異常終了時./
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    ELSE                                    /.正常終了時./
              /.##正常終了ｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K522'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    END

    CALL SCVMSG.TOKELIB,PARA-('ｼﾞｭｼﾝDT ﾍﾝｶﾝ   END  ')

/.##変換開始時刻取得#############################################./
HENTIMEE:

    ?STEP :=   'HENTIMEE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '# PDCM0020 ' && ?PARA1 && ' SYORINO=6 #'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

   /.##変換開始時間取得##./
    ?HTIME  := @STIME
    ?HTIMEE := %SBSTR(?HTIME,1,4)

/.##受信時間開始／終了、変換時間開始／終了更新##./
    CALL SCVMSG.TOKELIB,PARA-('ﾍﾝｶﾝ TIME SET       ')
DATESET:

    ?STEP :=   'DATESET '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '# PDCM0020 ' && ?PARA1 && ' SYORINO=7 #'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    /.##パラメタセット##./
    ?KBN     :=  '2'
    ?KJIKKO  :=  '0000000003'
    ?JIKKONM :=  '開始終了時刻更新　　　　　　　　　　　　'
    ?KEKKA   :=  ' '
    ?STATUS  :=  '    '

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KTDATE,?KTTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END

    OVRF    FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    CALL    PGM-NCV0170B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD,
                                       ?TIMES,?TIMEE,?HTIMES,?HTIMEE)
    ?PGMEC    :=    @PGMEC
    ?PGMECX   :=    %STRING(@PGMEC)
    ?PGMECN   :=    %SBSTR(?PGMECX,8,4)
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            /.##パラメタセット##./
            ?KBN     :=  '4'
            ?KEKKA   :=  '2'
            ?STATUS  :=  ?PGMECN
    ELSE
            /.##パラメタセット##./
            ?KBN     :=  '3'
            ?KEKKA   :=  '1'
            ?STATUS  :=  '    '
    END

    CALL DCM0000B.TOKSOLIB,PARA-(?KBN,?KTDATE,?KTTIME,?WSID,?TANTO,
                                 ?SYORI,?JIKKO,?KJIKKO,
                                 ?SYORINM,?JIKKONM,?KEKKA,?STATUS)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0      THEN
              GOTO ABEND
    END

    IF        ?PGMECN   ^=  '0000'  THEN    /.異常終了時./
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K519'
              GOTO ABEND
    END


/.##振分け件数ﾘｽﾄ出力##./
    CALL SCVMSG.TOKELIB,PARA-('ｹﾝｽｳ LST       START')
KENLST07:

    ?STEP :=   'KENLST  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '# PDCM0020 ' && ?PARA1 && ' SYORINO=8 #'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    SBMJOB JOB-?JOBNAME1,JOBD-CVCS.XUCL,JOBK-@B,PGM-KENLST.TOKCLIBO,
           VSIZE-5,RSIZE-16,LOG-@YES!1024,PARA-(?HIDUKE,?JIKAN,
           ?TOKCD,?LINE,?YUSEN)


/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    CALL SCVMSG.TOKELIB,PARA-('ｾｲｼﾞｮｳ ｼｭｳﾘｮｳ SJH310')
    ?MSGX := '# PDCM0020 ' && ?PARA1 && ' SYORINO=END OK #'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

   /.##ｴﾗｰﾘｽﾄ発行##./
    SBMJOB JOB-?JOBNAME2,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
           PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?PGCHK := '0'
/.  ?CLKEKA :=  '    '   ./
    RETURN    PGMEC-@PGMEC

/.##異常終了時##./
ABEND:

    ?ERKBN   :=  '022'
    ?ERDATE  :=  ?HIDUKE
    ?ERTIME  :=  ?JIKAN
    ?ERTKCD  :=  ?JTOKCD
    CALL ERR0010B.TOKSOLIB,PARA-(?ERKBN,?ERDATE,?ERTIME,
                                 ?ERTKCD)
    CALL SCVMSG.TOKELIB,PARA-('ｲｼﾞｮｳ ｼｭｳﾘｮｳ  SJH310')
    ?MSGX := '# PDCM0020 ' && ?PARA1 && ' SYORINO=END NG #'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

   /.##ｴﾗｰ結果更新##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB/TOKDTLIB
    CALL PGM-NCV0090B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
   /.##ｴﾗｰﾘｽﾄ発行##./
    SBMJOB JOB-?JOBNAME2,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-SNJ0590L.TOKELIBO,LOG-@YES!1024,PTY-5,PGMEL-@I/@L/
           @S/@T,LIBL-TOKFLIB/TOKELIB/TOKJLIB/TOKELIBO,
           PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '新ＤＣＭ発注取込の連続変換処理が取引先'
    ?KEKA2 :=  '毎変換処理が異常終了しました。'
    ?KEKA3 :=  'ログ出力しＮＡＶへ連絡して下さい'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END
    ?PGCHK := '1'
    ?KEKA := ?JKEKA
/.  ?CLKEKA := ?STATUS  ./

    RETURN    PGMEC-@PGMEC

```
