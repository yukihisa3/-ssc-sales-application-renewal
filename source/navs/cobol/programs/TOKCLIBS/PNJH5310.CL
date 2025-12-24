/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    変換伝票データ作成                   *  ./
/. *   JOB-ID      :    PNJH5310                             *  ./
/. *   JOB-NAME    :    ジョイフルエーケーＢＭＳ用　　　　　 *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TOKCD,P4-?LINE,P5-?YUSEN,
         P6-?LIBNM,P7-?FILNM,P8-?JKEKA)
/.  PGM        ./
/.##ﾊﾟﾗﾒﾀ定義##./
    PARA      ?HIDUKE   ,STRING*8,IN,VALUE-'        '   /.受信日付./
    PARA      ?JIKAN    ,STRING*4,IN,VALUE-'    '       /.受信時間./
    PARA    ?TOKCD    ,STRING*8,IN,VALUE-'        '   /.受信取引先./
    PARA      ?LINE     ,STRING*1,IN,VALUE-' '          /.回線./
    PARA      ?YUSEN    ,STRING*1,IN,VALUE-' '          /.回線優先./
    PARA      ?LIBNM    ,STRING*8,IN,VALUE-'        '   /.集信LIB./
    PARA      ?FILNM    ,STRING*8,IN,VALUE-'        '   /.集信FILE./
    PARA      ?JKEKA    ,STRING*4,OUT,VALUE-'    '      /.結果./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PNJH5310'  /.PROGRAM-ID./
    VAR       ?STEP     ,STRING*8                   /.STEP-ID./
/.##返品自動計上パラメタ##./
    VAR       ?JKBN     ,STRING*1,VALUE-' '         /.実行区分./
    VAR       ?BUMCD    ,STRING*4,VALUE-'2920'      /.部門ＣＤ./
    VAR       ?TANCD    ,STRING*2,VALUE-'98'        /.担当者ＣＤ./
    VAR       ?HTOKCD   ,STRING*8,VALUE-'00000000'  /.取引先ＣＤ./
    VAR       ?NDATE    ,STRING*8,VALUE-'        '  /.入力日./
    VAR       ?UPTIME   ,STRING*6,VALUE-'      '    /.更新日./
    VAR       ?DENCNT   ,STRING*7,VALUE-'0000000'   /.売伝追加件数./
    /. リスト出力用パラメタ ./
    VAR       ?KEIKBN   ,STRING*1,VALUE-'3'         /. 計上区分 ./
    VAR       ?TANST    ,STRING*2,VALUE-'98'        /. 担当者開始 ./
    VAR       ?TANED    ,STRING*2,VALUE-'98'        /. 担当者終了 ./
    VAR       ?DKBN     ,STRING*1,VALUE-'2'         /. 日付区分　 ./
    VAR       ?AFROM    ,STRING*8,VALUE-'00000000'  /. 検収日開始 ./
    VAR       ?ATO      ,STRING*8,VALUE-'00000000'  /. 検収日終了 ./
    VAR       ?NFROM    ,STRING*8,VALUE-'00000000'  /. 入力日開始 ./
    VAR       ?NTO      ,STRING*8,VALUE-'00000000'  /. 入力日終了 ./
    VAR       ?KFROM    ,STRING*8,VALUE-'00000000'  /. 計上日開始 ./
    VAR       ?KTO      ,STRING*8,VALUE-'00000000'  /. 計上日終了 ./
    VAR       ?SFROM    ,STRING*8,VALUE-'00000000'  /. 作成日開始 ./
    VAR       ?STO      ,STRING*8,VALUE-'00000000'  /. 作成日終了 ./
/.##ﾃﾞｰﾀ変換PG用ﾊﾟﾗﾒﾀ##./
    VAR       ?PARA     ,STRING*22,VALUE-'              '
/.##結果FLG用##./
    VAR       ?KEKA     ,STRING*4,VALUE-'    '      /.結果FLGﾊﾟﾗﾒﾀ./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR       ?LIBN     ,NAME                       /.ﾗｲﾌﾞﾗﾘ名前型./
    VAR       ?FILN     ,NAME                       /.ﾌｧｲﾙ名前型./
    VAR       ?FILLIB   ,NAME!MOD                   /.ﾌｧｲﾙ拡張用./
    VAR       ?FILID    ,STRING*17                  /.ﾌｧｲﾙ名表示用./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ジョイフルエーケーＢＭＳ取込変換処理'

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
SSY5310B:

    ?STEP :=   'SSY5310B'
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
    CALL      PGM-SSY5310B.TOKSOLIB,PARA-(?PARA)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              ?KEKA4 := 'データ変換処理（ＪＡＫ）'
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

    ?KEKA1 :=  'データ変換処理が異常終了しました。'
    ?KEKA2 :=  '受信状況を確認しログを採取し、ＮＡＶへ'
    ?KEKA3 :=  '連絡して下さい。'
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
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    ?JKEKA := ?KEKA

    RETURN    PGMEC-@PGMEC
