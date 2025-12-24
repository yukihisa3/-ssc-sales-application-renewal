# PNJH4407

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PNJH4407.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    変換伝票データ作成                   *  ./
/. *   JOB-ID      :    PNJH4407                             *  ./
/. *   JOB-NAME    :    島忠ＢＭＳ用　　　　　　　　　　　　 *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TOKCD,P4-?LINE,P5-?YUSEN,
         P6-?LIBNM,P7-?FILNM,P8-?JKEKA)
/.  PGM        ./
/.##ﾊﾟﾗﾒﾀ定義##./
    PARA    ?HIDUKE   ,STRING*8,IN,VALUE-'        '   /.受信日付./
    PARA    ?JIKAN    ,STRING*4,IN,VALUE-'    '       /.受信時間./
    PARA    ?TOKCD    ,STRING*8,IN,VALUE-'        '   /.受信取引先./
    PARA    ?LINE     ,STRING*1,IN,VALUE-' '          /.回線./
    PARA    ?YUSEN    ,STRING*1,IN,VALUE-' '          /.回線優先./
    PARA    ?LIBNM    ,STRING*8,IN,VALUE-'        '   /.集信LIB./
    PARA    ?FILNM    ,STRING*8,IN,VALUE-'        '   /.集信FILE./
    PARA    ?JKEKA    ,STRING*4,OUT,VALUE-'    '      /.結果./
/.  VAR     ?HIDUKE   ,STRING*8,VALUE-'20030313' ./  /.受信日付./
/.  VAR     ?JIKAN    ,STRING*4,VALUE-'1300'     ./  /.受信時間./
/.  VAR     ?TOKCD    ,STRING*8,VALUE-'        ' ./  /.受信取引先./
/.  VAR     ?LINE     ,STRING*1,VALUE-' '        ./  /.回線./
/.  VAR     ?YUSEN    ,STRING*1,VALUE-' '        ./  /.回線優先./
/.  VAR     ?LIBNM    ,STRING*8,VALUE-'ONLBLIB ' ./  /.集信LIB./
/.  VAR     ?FILNM    ,STRING*8,VALUE-'BJFL    ' ./  /.集信FILE./
/.  VAR     ?JKEKA    ,STRING*4,VALUE-'    '     ./  /.結果./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR     ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR     ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR     ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR     ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR     ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR     ?PGMID    ,STRING*8,VALUE-'PNJH4407'  /.PROGRAM-ID./
    VAR     ?STEP     ,STRING*8                   /.STEP-ID./
/.##ﾃﾞｰﾀ変換PG用ﾊﾟﾗﾒﾀ##./
    VAR     ?PARA     ,STRING*22,VALUE-'              '
/.##結果FLG用##./
    VAR     ?KEKA     ,STRING*4,VALUE-'    '      /.結果FLGﾊﾟﾗﾒﾀ./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR     ?LIBN     ,NAME                       /.ﾗｲﾌﾞﾗﾘ名前型./
    VAR     ?FILN     ,NAME                       /.ﾌｧｲﾙ名前型./
    VAR     ?FILLIB   ,NAME!MOD                   /.ﾌｧｲﾙ拡張用./
    VAR     ?FILID    ,STRING*17                  /.ﾌｧｲﾙ名表示用./

/.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾃﾞｰﾀ変換PGﾍのﾊﾟﾗﾒﾀ作成##./
    ?PARA :=   ?HIDUKE && ?JIKAN && ?TOKCD && ?LINE && ?YUSEN
    SNDMSG ?PARA,TO-XCTL.@ORGPROF,JLOG-@YES

/.##受信ﾌｧｲﾙノ編集##./
    ?FILN   :=  %NAME(?FILNM)
    ?LIBN   :=  %NAME(?LIBNM)
    ?FILLIB :=  %NCAT(?FILN,?LIBN)
    ?FILID  :=  %STRING(?FILLIB)
    SNDMSG ?FILID,TO-XCTL.@ORGPROF,JLOG-@YES

/.  データ変換                                                  ./
SSY4467B:

    ?STEP :=   'SSY4467B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/BMSFLIB/TOKKLIB/
              TOKDLIB/TOKWLIB/TOKSOLIB/TOKDTLIB

    OVRF      FILE-BMSHACL1,TOFILE-BMSHACL1.BMSFLIB
    OVRF      FILE-JSMKENL1,TOFILE-JSMKENL1.TOKJLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JHMRUTL1,TOFILE-JHMRUTL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JHSHENL1,TOFILE-JHSHENL1.TOKFLIB
    OVRVLDF   FILE-VLD500,TOFILE-LD500.XUCL
    CALL      PGM-SSY4467B.TOKSOLIB,PARA-(?PARA)
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

/.##受領データ共通変換##./
SJR0071B:

    ?STEP :=   'SJR0071B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0071B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受信エラーリスト（受領・返品）##./
SJR0220L:

    ?STEP :=   'SJR0220L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0220L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受信状況リスト（受領・返品）##./
SJR0210L:

    ?STEP :=   'SJR0210L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0210L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##返品データ抽出##./
SJR0280B:

    ?STEP :=   'SJR0280B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0280B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    ?JKEKA := ?KEKA
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
    ?JKEKA := ?KEKA

    RETURN    PGMEC-@PGMEC

```
