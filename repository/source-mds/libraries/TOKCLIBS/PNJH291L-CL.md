# PNJH291L

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PNJH291L.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    イオン流通ＢＭＳ変換処理             *  ./
/. *   JOB-ID      :    PNJH291L                             *  ./
/. *   JOB-NAME    :    イオン流通ＢＭＳ変換処理             *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TOKCD)
/.##ﾊﾟﾗﾒﾀ定義##./
    PARA      ?HIDUKE   ,STRING*8,IN,VALUE-'        ' /.受信日付./
    PARA      ?JIKAN    ,STRING*4,IN,VALUE-'    '     /.受信時間./
    PARA      ?TOKCD    ,STRING*8,IN,VALUE-'        ' /.取引先/
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PNJH291L'  /.PROGRAM-ID./
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

/.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
/.##DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/BMSFLIB/TOKCLIBO
##./DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/BMSFLIB/TOKCLIBO/
            TOKDLIB/TOKWLIB/TOKSOLIB/TOKDTLIB/TOKMDLIB

/.##イオン取引先振分マスタ未登録リスト発行##./
SBM1060L:

    ?STEP :=   'SBM1060L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-IONTERWK,TOFILE-IONTERWK.BMSFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SBM1060L.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K610'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.##イオン取引先別受信件数リスト発行##./
SBM1030L:

    ?STEP :=   'SBM1030L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-IONTWKL1,TOFILE-IONTWKL1.BMSFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SBM1030L.TOKELIBO,
              PARA-('1',?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K606'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.##イオン取引先別受信件数リスト発行##./
SBM1030A:

    ?STEP :=   'SBM1030A'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-IONTWKL1,TOFILE-IONTWKL1.BMSFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SBM1030L.TOKELIBO,
              PARA-('2',?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K606'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.##イオン取引先別受信件数リスト発行##./
SBM1030B:

    ?STEP :=   'SBM1030B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-IONTWKL1,TOFILE-IONTWKL1.BMSFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SBM1030L.TOKELIBO,
              PARA-('3',?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K606'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END
/.##イオン取引先別受信件数リスト発行##./
SBM1030C:

    ?STEP :=   'SBM1030C'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-IONTWKL1,TOFILE-IONTWKL1.BMSFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SBM1030L.TOKELIBO,
              PARA-('4',?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K606'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END
/.##イオン取引先別受信件数リスト発行##./
SBM1030D:

    ?STEP :=   'SBM1030D'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-IONTWKL1,TOFILE-IONTWKL1.BMSFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SBM1030L.TOKELIBO,
              PARA-('5',?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K606'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
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

    RETURN    PGMEC-@PGMEC

```
