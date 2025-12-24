/. ***********************************************************  ./
/. *     サカタのタネ　ＨＧ基幹システム　　　　　　          *  ./
/. *   SYSTEM-NAME :    送受信システム　　　　　　　　　　　 *  ./
/. *   JOB-ID      :    PSK00110                             *  ./
/. *   JOB-NAME    :    受領受信データ再取り込み    　       *  ./
/. ***********************************************************  ./
    PGM

/.##ﾊﾟﾗﾒﾀ定義##./
    VAR       ?HIDUKE   ,STRING*8,VALUE-'        ' /.受信日付./
    VAR       ?JIKAN    ,STRING*4,VALUE-'    '     /.受信時間./
    VAR       ?TOKCD    ,STRING*8,VALUE-'        ' /.受信取引先./
    VAR       ?LINE     ,STRING*1,VALUE-'I'        /.回線./
    VAR       ?YUSEN    ,STRING*1,VALUE-'1'        /.回線優先./
    VAR       ?LIBNM    ,STRING*8,VALUE-'ONLBLIB ' /.集信LIB./
    VAR       ?FILNM    ,STRING*8,VALUE-'ONLKEIJR' /.集信FILE./
    VAR       ?JKEKA    ,STRING*4,VALUE-'    '     /.結果./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PSK00110'  /.PROGRAM-ID./
    VAR       ?STEP     ,STRING*8                   /.STEP-ID./
    VAR       ?PGNM     ,STRING*40                  /.処理名 ./
    VAR       ?KEKA1    ,STRING*40                  /.ﾒｯｾｰｼﾞ1./
    VAR       ?KEKA2    ,STRING*40                  /.      2./
    VAR       ?KEKA3    ,STRING*40                  /.      3./
    VAR       ?KEKA4    ,STRING*40                  /.      4./
/.##ﾃﾞｰﾀ変換PG用ﾊﾟﾗﾒﾀ##./
    VAR       ?PARA     ,STRING*14,VALUE-'              '
/.##結果FLG用##./
    VAR       ?KEKA     ,STRING*4,VALUE-'    '      /.結果FLGﾊﾟﾗﾒﾀ./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR       ?LIBN     ,NAME                       /.ﾗｲﾌﾞﾗﾘ名前型./
    VAR       ?FILN     ,NAME                       /.ﾌｧｲﾙ名前型./
    VAR       ?FILLIB   ,NAME!MOD                   /.ﾌｧｲﾙ拡張用./
    VAR       ?FILID    ,STRING*17                  /.ﾌｧｲﾙ名表示用./

/.## ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ ##./
    ?PGNM :=  'ケーヨー　受領受信データ再取り込み'

STEP000:  /.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./

    ?STEP :=   'STEP000 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL LIBL-TOKELIB/TOKFLIB/TOKJLIB/TOKKLIB/
            ONLBLIB/TOKWLIB

STEP010:  /.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./

    ?STEP :=   'STEP010 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

STEP015:  /.## 再処理バッチ番号指定画面 ##./

    ?STEP :=   'STEP015 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIBO,MEDLIB-TOKELIBO
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    CALL      PGM-SSK0014I.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
        IF    ?PGMEC     =   4010 THEN
              ?MSGX :=  '## ｹｰﾖｰ受領再取込　取消終了 ##'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN    PGMEC-?PGMEC
        ELSE
              ?KEKA := 'K622' /.## ABENDｺｰﾄﾞｾｯﾄ ##./
              ?KEKA4 := '再処理バッチ番号指定'
              GOTO ABEND1
        END
    END

STEP016:  /.##当日スケジュールＭに”変換中”をセット##./

    ?STEP :=   'STEP016 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?KEKA := 'K522'   /.##変換中ｺｰﾄﾞｾｯﾄ##./
     OVRF FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
     CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA := 'K617' /.## ABENDｺｰﾄﾞｾｯﾄ ##./
              ?KEKA4 := '変換中コードセット'
              GOTO ABEND1
    END

STEP020:  /.##ﾃﾞｰﾀ変換PGﾍのﾊﾟﾗﾒﾀ作成##./

    ?STEP :=   'STEP020 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?PARA :=   ?HIDUKE && ?JIKAN && ?LINE && ?YUSEN
    SNDMSG ?PARA,TO-XCTL.@ORGPROF,JLOG-@YES

STEP030:  /.##受信ﾌｧｲﾙノ編集##./

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILN   :=  %NAME(?FILNM)
    ?LIBN   :=  %NAME(?LIBNM)
    ?FILLIB :=  %NCAT(?FILN,?LIBN)
    ?FILID  :=  %STRING(?FILLIB)
    SNDMSG ?FILID,TO-XCTL.@ORGPROF,JLOG-@YES

STEP040:  /.##前回バックアップデータ削除（受領データ）##./

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DLTFILE FILE-KEIJYRF.BAKFLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA := 'K613' /.## ABENDｺｰﾄﾞｾｯﾄ ##./
              ?KEKA4 := '前回データ削除（受領）'
              GOTO ABEND1
    END

STEP050:  /.##前回バックアップデータ削除（返品データ）##./

    ?STEP :=   'STEP050 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DLTFILE FILE-KEIJHRF.BAKFLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA := 'K614' /.## ABENDｺｰﾄﾞｾｯﾄ ##./
              ?KEKA4 := '前回データ削除（返品）'
              GOTO ABEND1
    END

STEP060:  /.##バックアップ処理（受領データ）##./

    ?STEP :=   'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CPYFILE FILE-KEIJYRF.TOKKLIB,TOFILE-KEIJYRF.BAKFLIB,
            CRTFILE-@YES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA := 'K615' /.## ABENDｺｰﾄﾞｾｯﾄ ##./
              ?KEKA4 := 'データバックアップ（受領）'
              GOTO ABEND1
    END

STEP070:  /.##バックアップ処理（返品データ）##./

    ?STEP :=   'STEP070 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CPYFILE FILE-KEIJHRF.TOKKLIB,TOFILE-KEIJHRF.BAKFLIB,
            CRTFILE-@YES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA := 'K616' /.## ABENDｺｰﾄﾞｾｯﾄ ##./
              ?KEKA4 := 'データバックアップ（返品）'
              GOTO ABEND1
    END
  /. TEST ./
  /.CALL TWAIT.XUCL ./
  /. TEST ./
STEP080:  /.##受信データ変換処理##./

    ?STEP :=   'STEP080 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-CVCSG001,TOFILE-?FILLIB
    OVRF      FILE-SHTDENL5,TOFILE-SHTDENL5.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-JHMRUTL1,TOFILE-JHMRUTL1.TOKFLIB
    OVRF      FILE-KEIJYOL1,TOFILE-KEIJYOL1.TOKKLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    OVRF      FILE-KEIJYRL1,TOFILE-KEIJYRL1.TOKKLIB
    CALL      PGM-SSK0010B.TOKELIBO,PARA-(?PARA)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA := 'K618'     /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA4 := '受領データ累積'
              GOTO ABEND2
    END

STEP090:  /.##受領返品データ抽出処理##./

    ?STEP :=   'STEP090 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-KEIJYRL1,TOFILE-KEIJYRL1.TOKKLIB
    OVRF      FILE-KEIJHRL1,TOFILE-KEIJHRL1.TOKKLIB
    OVRF      FILE-KEIJYOL1,TOFILE-KEIJYOL1.TOKKLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-SSK0011B.TOKELIBO,PARA-(?PARA)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA := 'K620'     /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA4 := '返品データ累積'
              GOTO ABEND3
    END

STEP100:  /.##受領データ取込エラーリスト発行##./

    ?STEP :=   'STEP100 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-KEIJYRL1,TOFILE-KEIJYRL1.TOKKLIB
    CALL      PGM-SSK0012L.TOKELIBO,PARA-(?PARA)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA := 'K611'     /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA4 := '受領データ取込エラーリスト'
              GOTO ABEND
    END

STEP110:  /.##受領データ取込件数リスト発行##./

    ?STEP :=   'STEP110 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-KEIJYOL1,TOFILE-KEIJYOL1.TOKKLIB
    CALL      PGM-SSK0013L.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA := 'K612'     /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA4 := '取込件数リスト'
              GOTO ABEND
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    ?KEKA  := 'K523'     /.##正常終了ｺｰﾄﾞｾｯﾄ##./
    ?JKEKA := ?KEKA
    ?PARA :=   ?HIDUKE && ?JIKAN && ?LINE && ?YUSEN
    SNDMSG ?PARA,TO-XCTL.@ORGPROF,JLOG-@YES
    /.##当日スケジュールマスタにエラーを更新する##./
    CALL PGM-SNJ0730B.TOKELIBO,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    /.##結果表示##./
    ?KEKA1 :=  '受領データ再取り込み　正常終了しました'
    ?KEKA2 :=  ''
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIBO,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RETURN    PGMEC-@PGMEC

/.##異常終了時##./
ABEND:

    /.##当日スケジュールマスタにエラーを更新する##./
    OVRF FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    CALL PGM-SNJ0730B.TOKELIBO,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    /.##上位ＣＬに結果コードを渡す              ##./
    ?JKEKA := ?KEKA
    /.##結果表示##./
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取しＮＡＶへ連絡して下'
    ?KEKA3 :=  'さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIBO,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    /.##異常終了メッセージを表示                ##./
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    RETURN    PGMEC-?PGMEC

/.##異常終了１##./
ABEND1:

    DEFLIBL   TOKELIB/TOKFLIB/TOKJLIB/TOKKLIB/ONLBLIB
    /.##当日スケジュールマスタにエラーを更新する##./
    CALL PGM-SNJ0730B.TOKELIBO,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    /.##上位ＣＬに結果コードを渡す              ##./
    ?MSGX :=  'ERR-CD = ' && %SBSTR(?PGMECX,8,4) && ' KEKA = ' &&
               ?KEKA
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?JKEKA := ?KEKA
    /.##結果表示##./
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取しＮＡＶへ連絡して下'
    ?KEKA3 :=  'さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIBO,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    /.##異常終了メッセージを表示                ##./
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    RETURN    PGMEC-?PGMEC

/.##異常終了２　受領累積データ変換処理時、データ復元##./
ABEND2:

    /.##バックアップデータを復元##./
    SETPF FILE-KEIJYRF.BAKFLIB,TOFILE-KEIJYRF.TOKKLIB,
          ADD-@NO,ACTCHK-@NO
    IF  @PGMEC    ^=   0    THEN
        ?KEKA := 'K619' /.## ABENDｺｰﾄﾞｾｯﾄ ##./
    END
    /.##当日スケジュールマスタにエラーを更新する##./
    CALL PGM-SNJ0730B.TOKELIBO,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    /.##上位ＣＬに結果コードを渡す              ##./
    ?JKEKA := ?KEKA
    /.##結果表示##./
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取しＮＡＶへ連絡して下'
    ?KEKA3 :=  'さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIBO,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    /.##異常終了メッセージを表示                ##./
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    RETURN    PGMEC-?PGMEC

/.##異常終了３　受領返品データ変換処理時、データ復元##./
ABEND3:

  /. TEST ./
  /.CALL TWAIT.XUCL  ./
  /. TEST ./
    /.##バックアップデータを復元##./
    SETPF FILE-KEIJHRF.BAKFLIB,TOFILE-KEIJHRF.TOKKLIB,
          ADD-@NO,ACTCHK-@NO
    IF  @PGMEC    ^=   0    THEN
        ?KEKA := 'K621' /.## ABENDｺｰﾄﾞｾｯﾄ ##./
    END
    /.##当日スケジュールマスタにエラーを更新する##./
    CALL PGM-SNJ0730B.TOKELIBO,
         PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    /.##上位ＣＬに結果コードを渡す              ##./
    ?JKEKA := ?KEKA
              /.１２３４５６７８９０１２３４５６７８９０./
    /.##結果表示##./
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取しＮＡＶへ連絡して下'
    ?KEKA3 :=  'さい。'
    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIBO,MEDLIB-TOKELIB
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    /.##異常終了メッセージを表示                ##./
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    RETURN    PGMEC-?PGMEC
