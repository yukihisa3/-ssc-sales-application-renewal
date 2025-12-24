/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    変換伝票データ作成                   *  ./
/. *   JOB-ID      :    PNJH4503                             *  ./
/. *   JOB-NAME    :    イオン（統合）　　　　　　　　　　　 *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TOKCD,P4-?LINE,P5-?YUSEN,
         P6-?LIBNM,P7-?FILNM,P8-?JKEKA)
/.##ﾊﾟﾗﾒﾀ定義##./
    PARA      ?HIDUKE   ,STRING*8,IN,VALUE-'        ' /.受信日付./
    PARA      ?JIKAN    ,STRING*4,IN,VALUE-'    '     /.受信時間./
    PARA      ?TOKCD    ,STRING*8,IN,VALUE-'        ' /.受信取引先./
    PARA      ?LINE     ,STRING*1,IN,VALUE-' '        /.回線./
    PARA      ?YUSEN    ,STRING*1,IN,VALUE-' '        /.回線優先./
    PARA      ?LIBNM    ,STRING*8,IN,VALUE-'        ' /.集信LIB./
    PARA      ?FILNM    ,STRING*8,IN,VALUE-'        ' /.集信FILE./
    PARA      ?JKEKA    ,STRING*4,OUT,VALUE-'    '    /.結果./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PNJH4503'  /.PROGRAM-ID./
    VAR       ?STEP     ,STRING*8                   /.STEP-ID./
/.##ﾃﾞｰﾀ変換PG用ﾊﾟﾗﾒﾀ##./
    VAR       ?PARA     ,STRING*14,VALUE-'              '
/.##結果FLG用##./
    VAR       ?KEKA     ,STRING*4,VALUE-'    '      /.結果FLGﾊﾟﾗﾒﾀ./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR       ?LIBN     ,NAME                       /.ﾗｲﾌﾞﾗﾘ名前型./
    VAR       ?FILN     ,NAME                       /.ﾌｧｲﾙ名前型./
    VAR       ?FILLIB   ,NAME!MOD                   /.ﾌｧｲﾙ拡張用./
    VAR       ?FILID    ,STRING*17                  /.ﾌｧｲﾙ名表示用./
    VAR       ?SKBN     ,STRING*1,VALUE-'2'         /.帳票印字KBN./
    VAR       ?CHK      ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK1     ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK2     ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK3     ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?OK1      ,STRING*2,VALUE-'  '        /.実行確認1./
    VAR       ?OK2      ,STRING*2,VALUE-'  '        /.実行確認2./
    VAR       ?OK3      ,STRING*2,VALUE-'  '        /.実行確認3./

/.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##ﾃﾞｰﾀ変換PGﾍのﾊﾟﾗﾒﾀ作成##./
    ?PARA :=   ?HIDUKE && ?JIKAN && ?LINE && ?YUSEN
    SNDMSG ?PARA,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##受信ﾌｧｲﾙノ編集##./
    ?FILN   :=  %NAME(?FILNM)
    ?LIBN   :=  %NAME(?LIBNM)
    ?FILLIB :=  %NCAT(?FILN,?LIBN)
    ?FILID  :=  %STRING(?FILLIB)
    SNDMSG ?FILID,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##受信データ振分##./
SJH4550B:

    ?STEP :=   'SJH4550B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB

    OVRF      FILE-CVCS256,TOFILE-?FILLIB
    OVRF      FILE-DATAF1,TOFILE-IONHAC1.TOKFLIB
    OVRF      FILE-DATAF2,TOFILE-IONHAC2.TOKFLIB
    OVRF      FILE-DATAF3,TOFILE-IONHAC3.TOKFLIB
    OVRF      FILE-DATAF4,TOFILE-IONHAC4.TOKFLIB
    OVRF      FILE-DATAF5,TOFILE-IONHAC5.TOKFLIB
    OVRF      FILE-DATAF6,TOFILE-IONHAC6.TOKFLIB
    CALL      PGM-SJH4550B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K522'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    ELSE
              /.##正常終了ｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K522'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    END

/.##受信データ振分##./
SJH4551B:

    ?STEP :=   'SJH4551B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB

    OVRF      FILE-CVCS256,TOFILE-IONHAC6.TOKFLIB
    OVRF      FILE-DATAF1,TOFILE-JDATAF1.TOKFLIB
    OVRF      FILE-DATAF2,TOFILE-JDATAF2.TOKFLIB
    OVRF      FILE-DATAF3,TOFILE-JDATAF3.TOKFLIB
    OVRF      FILE-DATAF4,TOFILE-JDATAF4.TOKFLIB
    OVRF      FILE-DATAF5,TOFILE-JDATAF5.TOKFLIB
    OVRF      FILE-DATAF6,TOFILE-JDATAF6.TOKFLIB
    OVRF      FILE-DATAF7,TOFILE-JDATAF7.TOKFLIB
    OVRF      FILE-DATAF8,TOFILE-JDATAF8.TOKFLIB
    OVRF      FILE-DATAF9,TOFILE-JDATAF9.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SJH4551B.TOKELIB
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

/.##振分リスト発行##./
SJH4559L:

    ?STEP :=   'SJH4559L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB

    OVRF      FILE-HOU,TOFILE-JDATAF1.TOKFLIB
    OVRF      FILE-FKU,TOFILE-JDATAF2.TOKFLIB
    OVRF      FILE-SEU,TOFILE-JDATAF3.TOKFLIB
    OVRF      FILE-OKU,TOFILE-JDATAF4.TOKFLIB
    OVRF      FILE-HKU,TOFILE-JDATAF5.TOKFLIB
    OVRF      FILE-OSU,TOFILE-JDATAF6.TOKFLIB
    OVRF      FILE-YOBI1,TOFILE-JDATAF7.TOKFLIB
    OVRF      FILE-YOBI2,TOFILE-JDATAF8.TOKFLIB
    OVRF      FILE-YOBI3,TOFILE-JDATAF9.TOKFLIB
    CALL      PGM-SJH4559L.TOKELIB,PARA-(?SKBN)
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

/.##データ変換##./
NJH4555B:

    ?STEP :=   'NJH4555B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB

    OVRF      FILE-CVCSG001,TOFILE-JDATAF1.TOKFLIB
    OVRF      FILE-JSMKENL1,TOFILE-JSMKENL1.TOKJLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-JHMRUTL1,TOFILE-JHMRUTL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JHSHENL1,TOFILE-JHSHENL1.TOKFLIB
    OVRVLDF   FILE-VLD500,TOFILE-LD500.XUCL
    CALL      PGM-NJH4555B.TOKELIBO,PARA-(?PARA)
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
        CNVFILE FILE-JDATAF3.TOKFLIB,
             TOFILE-IONHACS.TOKFLIB,ADD-@NO,BF-1
        IF  @PGMEC    ^=   0    THEN
            /.##ABENDｺｰﾄﾞｾｯﾄ##./
            ?KEKA := 'K555'
            CALL PGM-SNJ0730B.TOKELIBO,
                 PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
            GOTO ABEND
        END
        CNVFILE FILE-JDATAF3.TOKFLIB,
            TOFILE-IONHACS.ONLBLIB,ADD-@NO,BF-1
        IF  @PGMEC    ^=   0    THEN
            /.##ABENDｺｰﾄﾞｾｯﾄ##./
            ?KEKA := 'K555'
            CALL PGM-SNJ0730B.TOKELIBO,
                 PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
            GOTO ABEND
        END
        CNVFILE FILE-JDATAF2.TOKFLIB,
            TOFILE-IONHACK.TOKFLIB,ADD-@NO,BF-1
        IF  @PGMEC    ^=   0    THEN
            /.##ABENDｺｰﾄﾞｾｯﾄ##./
            ?KEKA := 'K555'
            CALL PGM-SNJ0730B.TOKELIBO,
                 PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
            GOTO ABEND
        END
        CNVFILE FILE-JDATAF6.TOKFLIB,
            TOFILE-IONHACO.TOKFLIB,ADD-@NO,BF-1
        IF  @PGMEC    ^=   0    THEN
            /.##ABENDｺｰﾄﾞｾｯﾄ##./
            ?KEKA := 'K555'
            CALL PGM-SNJ0730B.TOKELIBO,
                 PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
            GOTO ABEND
        END
        CNVFILE FILE-JDATAF6.TOKFLIB,
            TOFILE-IONHACO.ONLBLIB,ADD-@NO,BF-1
        IF  @PGMEC    ^=   0    THEN
            /.##ABENDｺｰﾄﾞｾｯﾄ##./
            ?KEKA := 'K555'
            CALL PGM-SNJ0730B.TOKELIBO,
                 PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
            GOTO ABEND
        END
        CNVFILE FILE-JDATAF4.TOKFLIB,
            TOFILE-IONHACSS.ONLBLIB,ADD-@NO,BF-1
        IF  @PGMEC    ^=   0    THEN
            /.##ABENDｺｰﾄﾞｾｯﾄ##./
            ?KEKA := 'K555'
            CALL PGM-SNJ0730B.TOKELIBO,
                 PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
            GOTO ABEND
        END
    END

/.##受信ＤＴ存在チェック##./
IONCHK1:

    ?STEP :=   'IONCHK1 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    OVRF      FILE-JDATAF,TOFILE-IONHACS.ONLBLIB
    CALL      PGM-SCV1004B.OSKELIB,PARA-(?CHK1)
    IF        @PGMEC    ^=   0    THEN
                    GOTO ABEND
    END

    OVRF      FILE-JDATAF,TOFILE-IONHACO.ONLBLIB
    CALL      PGM-SCV1004B.OSKELIB,PARA-(?CHK2)
    IF        @PGMEC    ^=   0    THEN
                    GOTO ABEND
    END

    OVRF      FILE-JDATAF,TOFILE-IONHACSS.ONLBLIB
    CALL      PGM-SCV1004B.OSKELIB,PARA-(?CHK3)
    IF        @PGMEC    ^=   0    THEN
                    GOTO ABEND
    END

/.##イオン仙台　変換##./
IONSEN:

    ?STEP :=   'IONSEN  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    IF    ?CHK1  =  '1'   THEN
       SNDMSG MSG-'**ｲｵﾝ ｾﾝﾀﾞｲ 変換 START**',TO-XCTL
       ,SLOG-@YES,JLOG-@YES
       CALL PNJH4590.TOKCLIBO,PARA-(?OK1)
       IF   ?OK1  =  'NG'  THEN
       GOTO ABEND END
    ELSE
       SNDMSG MSG-'**ｲｵﾝ ｾﾝﾀﾞｲ ﾃﾞｰﾀなし  **',TO-XCTL
      ,SLOG-@YES,JLOG-@YES
    END

/.##イオン西日本変換##./
IONNIS:

    ?STEP :=   'IONNIS  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    IF    ?CHK2  =  '1'   THEN
          SNDMSG MSG-'**ｲｵﾝ ﾆｼﾆﾎﾝ 変換 START**',TO-XCTL
          ,SLOG-@YES,JLOG-@YES
          CALL PNJH4591.TOKCLIBO,PARA-(?OK2)
          IF   ?OK1  =  'NG'  THEN
          GOTO ABEND END
    ELSE
          SNDMSG MSG-'**ｲｵﾝ ﾆｼﾆﾎﾝ ﾃﾞｰﾀなし  **',TO-XCTL
          ,SLOG-@YES,JLOG-@YES
    END

/.##イオンＳＰ　変換##./
IONSP:

    ?STEP :=   'IONSP   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

    IF    ?CHK3  =  '1'   THEN
          SNDMSG MSG-'**ｲｵﾝ SP    変換 START**',TO-XCTL
          ,SLOG-@YES,JLOG-@YES
          DLTOVRF
          CALL PNJH4592.TOKCLIBO,PARA-(?OK3)
          IF   ?OK1  =  'NG'  THEN
          GOTO ABEND END
    ELSE
          SNDMSG MSG-'**ｲｵﾝ SP    ﾃﾞｰﾀなし  **',TO-XCTL
          ,SLOG-@YES,JLOG-@YES
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    ?JKEKA := ?KEKA
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
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
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END
    ?JKEKA := ?KEKA

    RETURN    PGMEC-@PGMEC
