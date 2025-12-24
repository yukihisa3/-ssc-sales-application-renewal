# PNJH4202

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PNJH4202.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    変換伝票データ作成＋ＴＣ発注データ   *  ./
/. *   JOB-ID      :    PSJH4202                             *  ./
/. *   JOB-NAME    :    ジュンテンドー　　　                 *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TOKCD,P4-?LINE,P5-?YUSEN,
         P6-?LIBNM,P7-?FILNM,P8-?JKEKA)
/.##ﾊﾟﾗﾒﾀ定義##./
    PARA      ?HIDUKE   ,STRING*8,IN,VALUE-'        '
    PARA      ?JIKAN    ,STRING*4,IN,VALUE-'    '
    PARA      ?TOKCD    ,STRING*8,IN,VALUE-'        '
    PARA      ?LINE     ,STRING*1,IN,VALUE-' '
    PARA      ?YUSEN    ,STRING*1,IN,VALUE-' '
    PARA      ?LIBNM    ,STRING*8,IN,VALUE-'        '
    PARA      ?FILNM    ,STRING*8,IN,VALUE-'        '
    PARA      ?JKEKA    ,STRING*4,OUT,VALUE-'    '
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PSJH4202'  /.PROGRAM-ID./
    VAR       ?STEP     ,STRING*8                   /.STEP-ID./
/.##ﾃﾞｰﾀ変換PG用ﾊﾟﾗﾒﾀ##./
    VAR       ?PARA     ,STRING*14,VALUE-'              '
/.##TC納品用ﾊﾟﾗﾒﾀ##./
    VAR       ?PARA1    ,STRING*15,VALUE-'               '
    VAR       ?CHK      ,STRING*1,VALUE-' '
/.##結果FLG用##./
    VAR       ?KEKA     ,STRING*4,VALUE-'    '      /.結果FLGﾊﾟﾗﾒﾀ./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR       ?LIBN     ,NAME                       /.ﾗｲﾌﾞﾗﾘ名前型./
    VAR       ?FILN     ,NAME                       /.ﾌｧｲﾙ名前型./
    VAR       ?FILLIB   ,NAME!MOD                   /.ﾌｧｲﾙ拡張用./
    VAR       ?FILID    ,STRING*17                  /.ﾌｧｲﾙ名表示用./
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./
/.##時間+１分##./
    VAR       ?JIKAN2   ,STRING*4,VALUE-'    '

/.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾃﾞｰﾀ変換PGﾍのﾊﾟﾗﾒﾀ作成##./
    ?PARA :=   ?HIDUKE && ?JIKAN && ?LINE && ?YUSEN
    SNDMSG ?PARA,TO-XCTL.@ORGPROF,JLOG-@YES

/.##受信ﾌｧｲﾙノ編集##./
    ?FILN   :=  %NAME(?FILNM)
    ?LIBN   :=  %NAME(?LIBNM)
    ?FILLIB :=  %NCAT(?FILN,?LIBN)
    ?FILID  :=  %STRING(?FILLIB)
    SNDMSG ?FILID,TO-XCTL.@ORGPROF,JLOG-@YES

/.##データ変換（店納品データ＋ＴＣ発注データ＋ＴＣ受領データ）##./
SJH4202B:

    ?STEP :=   'SJH4202B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/OSKELIB/OSKFLIB

    OVRF      FILE-CVCSG001,TOFILE-?FILLIB
    OVRF      FILE-JSMKENL1,TOFILE-JSMKENL1.TOKJLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JHMRUTL1,TOFILE-JHMRUTL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JHSHENL1,TOFILE-JHSHENL1.TOKFLIB
    OVRF      FILE-JUNTENWK,TOFILE-JUNTENWK.OSKFLIB
    OVRF      FILE-JUNTENJR,TOFILE-JUNTENJR.OSKFLIB
    OVRVLDF   FILE-VLD500,TOFILE-LD500.XUCL
    CALL      PGM-NJH4202B.TOKELIBO,PARA-(?PARA)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := '92'
              GOTO ABEND
    ELSE
              /.##正常終了ｺｰﾄﾞｾｯﾄ##./
              ?KEKA := '03'
    END

/.##時間＋１分##./
SJT0300B:

    ?STEP :=   'SJT0300B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJT0300B.OSKELIB,PARA-(?JIKAN,?JIKAN2)
    IF        @PGMEC    ^=   0    THEN
              /.##異常の場合、同一時間をセットする##./
              ?MSGX :=  '##時間＋１分算出エラー　同一時間で処理##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              ?JIKAN2 := ?JIKAN
              ?MSGX :=  '##ＴＣ受信時間 = ' && ?JIKAN2 && ' ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END

/.##データ変換（ＴＣ発注データ）##./
SJT0010B:

    ?STEP :=   'SJT0010B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/OSKELIB/OSKFLIB

    OVRF      FILE-JUNTENWK,TOFILE-JUNTENWK.OSKFLIB
    OVRF      FILE-JTCKENL1,TOFILE-JTCKENL1.OSKFLIB
    OVRF      FILE-JTCTJSL1,TOFILE-JTCTJSL1.OSKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JHMRUTL1,TOFILE-JHMRUTL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JTCHEDL1,TOFILE-JTCHEDL1.OSKFLIB
    OVRF      FILE-JTCMEIL1,TOFILE-JTCMEIL1.OSKFLIB
    OVRF      FILE-JTCMEOL1,TOFILE-JTCMEOL1.OSKFLIB
    CALL      PGM-SJT0010B.OSKELIB,PARA-(?HIDUKE,?JIKAN2,?LINE,?YUSEN,
                                         ?CHK)
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

/.##在庫引当（ＴＣ発注データ）##./
SJT0180B:

    ?STEP :=   'SJT0180B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-JTCHEDL1,TOFILE-JTCHEDL1.OSKFLIB
    OVRF      FILE-JTCMEIL1,TOFILE-JTCMEIL1.OSKFLIB
    OVRF      FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JHMTJSL1,TOFILE-JTCTJSL1.TOKFLIB
    CALL      PGM-SJT0180B.OSKELIB,PARA-(?HIDUKE,?JIKAN2,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN2,?TOKCD,?KEKA)
              GOTO ABEND
    ELSE
              /.##正常終了ｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K522'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN2,?TOKCD,?KEKA)
    END

/.##件数チェック＋確認リスト発行##./
PCHK:

    ?STEP :=   'PCHK    '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?MSGX :=   '## CHK = ' && ?CHK && ' ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    IF  ?CHK  =  '1'  THEN
        ?PGNM  :=  'ＴＣ納品データ変換処理'
        ?KEKA1 :=  'ＴＣ納品データの変換が正常終了しました'
        ?KEKA2 :=  '今回の受信でＴＣ納品データの発注があり'
        ?KEKA3 :=  'ます。件数リストを確認して下さい。'
        ?KEKA4 :=  '【ＴＣ納品データ有り】'
        CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
        IF   @PGMEC ^= 0 THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN2,?TOKCD,?KEKA)
              GOTO      ABEND
        END
        OVRF    FILE-JHMKENL1,TOFILE-JTCKENL1.OSKFLIB
        OVRF    FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
        OVRF    FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
        OVRF    FILE-JHMEOSL1,TOFILE-JHMEOSL1.TOKFLIB
        CALL    PGM-SJT9996L.OSKELIB,PARA-(?HIDUKE,?JIKAN2,?TOKCD)
        IF   @PGMEC ^= 0 THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN2,?TOKCD,?KEKA)
              GOTO      ABEND
        END
        /.##物流連携用データ作成##./
        ?MSGX :=  '## 物流連携用CHK用DT削除 ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        CLRFILE FILE-JTCHKYF.TOKWLIB
        IF   @PGMEC ^= 0 THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN2,?TOKCD,?KEKA)
              GOTO      ABEND
        END
        ?MSGX :=  '## 物流連携用DT作成 ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        OVRF    FILE-JTCHEDL1,TOFILE-JTCHEDL1.OSKFLIB
        OVRF    FILE-JTCMEIL1,TOFILE-JTCMEIL1.OSKFLIB
        OVRF    FILE-JTCMEOL1,TOFILE-JTCMEOL1.OSKFLIB
        OVRF    FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
        OVRF    FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
        OVRF    FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
        OVRF    FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
        OVRF    FILE-JTCDENL1,TOFILE-JTCDENL1.TOKKLIB
        OVRF    FILE-JTCHKYL1,TOFILE-JTCHKYL1.TOKWLIB
        CALL    PGM-SBT0035B.TOKELIBO,PARA-(?HIDUKE,?JIKAN2,?TOKCD)
        IF   @PGMEC ^= 0 THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN2,?TOKCD,?KEKA)
              GOTO      ABEND
        END
        ?MSGX :=  '## 連携ＦＬＧ初期化 ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        OVRF    FILE-SHTDENLJ,TOFILE-JTCDENL2.TOKKLIB
        OVRF    FILE-LNKONLL1,TOFILE-LNKONLL1.TOKKLIB
        OVRF    FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
        OVRF    FILE-ZSOKSMS1,TOFILE-ZSOKMS1.TOKFLIB
        CALL    PGM-SBT0940B.TOKELIBO,PARA-(?HIDUKE,?JIKAN2,?TOKCD)
        IF   @PGMEC ^= 0 THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN2,?TOKCD,?KEKA)
              GOTO      ABEND
        END
    ELSE
        ?PGNM  :=  'ＴＣ納品データ変換処理'
        ?KEKA1 :=  'ＴＣ納品データの変換が正常終了しました'
        ?KEKA2 :=  '今回の受信でＴＣ納品データの発注はあり'
        ?KEKA3 :=  'ませんでした。'
        ?KEKA4 :=  '【ＴＣ納品データ無し】'
        CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
        IF   @PGMEC ^= 0 THEN
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN2,?TOKCD,?KEKA)
              GOTO      ABEND
        END
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    ?KEKA := ?JKEKA
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
    ?KEKA := ?JKEKA

    RETURN    PGMEC-@PGMEC

```
