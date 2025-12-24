/. ***********************************************************  ./
/. *   サカタのタネ　　本社システム                          *  ./
/. *   SYSTEM-NAME :    流通ＢＭＳ　　　　　　               *  ./
/. *   JOB-ID      :    PNJH3204                             *  ./
/. *   JOB-NAME    :    売上データ変換処理（カインズ）　　　 *  ./
/. *                    受信待受処理(CBMSJYU)より起動　　　　*  ./
/. *   CREATE      :    2014/05/29                           *  ./
/. *   UPDATE      :        /  /                             *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TOKCD,P4-?LINE,P5-?YUSEN,
         P6-?LIBNM,P7-?FILNM,P8-?JKEKA)
/.  PGM  ←単独実行テスト用      ./

/.##パラメタ定義##./
    PARA    ?HIDUKE   ,STRING*8,IN,VALUE-'        '  /.受信日付./
    PARA    ?JIKAN    ,STRING*4,IN,VALUE-'    '      /.受信時間./
    PARA    ?TOKCD    ,STRING*8,IN,VALUE-'        '  /.受信取引先./
    PARA    ?LINE     ,STRING*1,IN,VALUE-' '         /.回線./
    PARA    ?YUSEN    ,STRING*1,IN,VALUE-' '         /.回線優先./
    PARA    ?LIBNM    ,STRING*8,IN,VALUE-'        '  /.集信LIB./
    PARA    ?FILNM    ,STRING*8,IN,VALUE-'        '  /.集信FILE./
    PARA    ?JKEKA    ,STRING*4,OUT,VALUE-'    '     /.結果./
/.##↓パラメタ定義（単独実行テスト用）##./
/.  VAR     ?HIDUKE   ,STRING*8,VALUE-'20030313' ./  /.受信日付./
/.  VAR     ?JIKAN    ,STRING*4,VALUE-'1300'     ./  /.受信時間./
/.  VAR     ?TOKCD    ,STRING*8,VALUE-'        ' ./  /.受信取引先./
/.  VAR     ?LINE     ,STRING*1,VALUE-' '        ./  /.回線./
/.  VAR     ?YUSEN    ,STRING*1,VALUE-' '        ./  /.回線優先./
/.  VAR     ?LIBNM    ,STRING*8,VALUE-'ONLBLIB ' ./  /.集信LIB./
/.  VAR     ?FILNM    ,STRING*8,VALUE-'BJFL    ' ./  /.集信FILE./
/.  VAR     ?JKEKA    ,STRING*4,VALUE-'    '     ./  /.結果./
/.##ワーク定義##./
    VAR     ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR     ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR     ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR     ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR     ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR     ?PGMID    ,STRING*8,VALUE-'PNJH3204'  /.PROGRAM-ID./
    VAR     ?STEP     ,STRING*8                   /.STEP-ID./
/.##データ変換ＰＧ引渡しパラメタ##./
    VAR     ?PARA     ,STRING*22,VALUE-'              '
/.##結果FLG用##./
    VAR     ?KEKA     ,STRING*4,VALUE-'    '      /.結果FLGﾊﾟﾗﾒﾀ./
/.##非使用：ファイルＩＤ変換ワーク##./
  /.VAR     ?LIBN     ,NAME       ./              /.ﾗｲﾌﾞﾗﾘ名前型./
  /.VAR     ?FILN     ,NAME       ./              /.ﾌｧｲﾙ名前型./
  /.VAR     ?FILLIB   ,NAME!MOD   ./              /.ﾌｧｲﾙ拡張用./
  /.VAR     ?FILID    ,STRING*17  ./              /.ﾌｧｲﾙ名表示用./
/.----------------------------------------------------------------./
/.##開始メッセージ表示##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##データ変換ＰＧへのパラメタ編集##./
    ?PARA :=   ?HIDUKE && ?JIKAN && ?TOKCD && ?LINE && ?YUSEN
    SNDMSG ?PARA,TO-XCTL.@ORGPROF,JLOG-@YES

/.##非使用：ＩＮＰＵＴ（受信ファイル）ＩＤ編集##./
  /.?FILN   :=  %NAME(?FILNM)                 ./
  /.?LIBN   :=  %NAME(?LIBNM)                 ./
  /.?FILLIB :=  %NCAT(?FILN,?LIBN)            ./
  /.?FILID  :=  %STRING(?FILLIB)              ./
  /.SNDMSG ?FILID,TO-XCTL.@ORGPROF,JLOG-@YES  ./

/.  売上データ変換処理                                          ./
NJH3204B:

    ?STEP :=   'NJH3204B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/BMSFLIB

    OVRF      FILE-BMSHACL1,TOFILE-BMSHACL1.BMSFLIB
    OVRF      FILE-JSMKENL1,TOFILE-JSMKENL1.TOKJLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JHMRUTL1,TOFILE-JHMRUTL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JHSHENL1,TOFILE-JHSHENL1.TOKFLIB
    OVRVLDF   FILE-VLD500,TOFILE-LD500.XUCL
    CALL      PGM-NJH3204B.TOKELIBO,PARA-(?PARA)
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
