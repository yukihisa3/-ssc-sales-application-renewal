****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　日次振替　　　　　　　　　　　　　*
*    作成日／更新日　　　：　00/06/20                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　電算室振替データ更新Ｆを読んで、　*
*                            在庫マスタを更新，又は追加する。　*
*                                                              *
*    94.08.02            :   富士通側締日以降に前月分データが  *
*                        :   渡された場合前月分データの更新を  *
*                        :   行う　　　　　　                  *
*    09.10.06            :   在庫改善／販売実績　　　　　　　  *
*                        :   在庫更新時、_番をマスタより取得  *
*                        :   して更新を行なう。発注更新追加。  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SFU0120B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<< 電算室振替データ更新ファイル       >>---*
     SELECT   FURIKAF   ASSIGN    TO        DA-01-VI-FURIKAL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   FUR-F01
                                                 FUR-F02
                                                 FUR-F03
                                                 FUR-F04
                                                 FUR-F05
                        FILE    STATUS      IS   FUR-STATUS.
*
*---<<  ＴＯＫＵファイル（エラー分）---*
     SELECT   FURIKAE   ASSIGN    TO        DA-01-S-FURIKAE
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   FUE-STATUS.
*
*---<<  商品在庫マスタ　  >>---*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F021
                                                 ZAI-F022
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-STATUS.
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*---<< 商品名称マスタ >>-*
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
*---<< 商品変換ＴＢＬ >>-*
     SELECT   SHOTBL12  ASSIGN    TO        DA-01-VI-SHOTBL12
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TBL-F04
                                                 TBL-F031
                                                 TBL-F032
                        FILE      STATUS    IS   TBL-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＴＯＫＵファイル  >>---*
 FD    FURIKAF.
       COPY   FURIKAF   OF        XFDLIB
              JOINING   FUR       PREFIX.
*---<<  ＴＯＫＵファイル  >>---*
 FD    FURIKAE.
       COPY   FURIKAF   OF        XFDLIB
              JOINING   FUE       PREFIX.
*---<<  商品在庫マスタ 　 >>---*
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*----<< 条件ファイル >>-*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<  商品名称マスタ >>-*
 FD  MEIMS1.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  商品変換ＴＢＬ >>-*
 FD  SHOTBL12.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  FUR-STATUS          PIC  X(02).
     02  FUE-STATUS          PIC  X(02).
     02  ZAI-STATUS          PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  TBL-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  SHOTBL12-INV-FLG    PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE ZERO.
     02  REWRITE-CNT         PIC  9(07)  VALUE ZERO.
     02  WRITE-CNT           PIC  9(07)  VALUE ZERO.
     02  ERR-CNT             PIC  9(07)  VALUE ZERO.
     02  WK-ZAI03            PIC  X(06)  VALUE SPACE.
****  ＷＲＫ領域  ***
 01  WRK-AREA.
     02  WRK-DATE1           PIC  9(06).
     02  WRK-DATE1R          REDEFINES   WRK-DATE1.
         04  WRK-DATE1R1     PIC  9(04).
         04  WRK-DATE1R2     PIC  9(02).
     02  WRK-DATE2           PIC  9(06).
     02  SYS-DATE            PIC  9(06)  VALUE  ZERO.
     02  SYS-DATE-YMD        PIC  9(08)  VALUE  ZERO.
     02  WK-FUR-F13          PIC  9(09)V99 VALUE  ZERO.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SFU0120B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    FURIKAF.
     MOVE     "FURIKAF"         TO   ERR-FL-ID.
     MOVE     FUR-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZAMZAIF.
     MOVE     "ZAMZAIF"      TO   ERR-FL-ID.
     MOVE     ZAI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"      TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    MEIMS1.
     MOVE     "MEIMS1 "      TO   ERR-FL-ID.
     MOVE     MEI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC5                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    FURIKAE.
     MOVE    "FURIKAE"       TO   ERR-FL-ID.
     MOVE     FUE-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC6                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    SHOTBL12.
     MOVE    "SHOTBL12"      TO   ERR-FL-ID.
     MOVE     TBL-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 KEI0100-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN     INPUT          HJYOKEN  MEIMS1 SHOTBL12
              I-O            FURIKAF  ZAMZAIF
              OUTPUT         FURIKAE.
*システム日付の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE-YMD.
*---<  日付の取得  >---*
     MOVE    "99"            TO   JYO-F01.
     MOVE    "ZAI"           TO   JYO-F02.
     READ    HJYOKEN
     END-READ.
     MOVE    JYO-F05         TO   WRK-DATE1.
*
     ADD             1               TO   WRK-DATE1.
     IF      WRK-DATE1R2     >       12
             MOVE    1               TO   WRK-DATE1R2
             ADD     1               TO   WRK-DATE1R1
     END-IF.
*
     DISPLAY "WRK-DATE1 = " WRK-DATE1 UPON CONS.
*
     PERFORM    READ-FURIKAF-SEC.
 INIT-END.
     EXIT.
****************************************************************
*      _１　　ＴＯＫＵファイルＲＥＡＤ処理                    *
****************************************************************
 READ-FURIKAF-SEC               SECTION.
     READ    FURIKAF
         AT   END
           MOVE   "END"      TO   END-FLG
           GO                TO   READ-FURIKAF-END
     END-READ.
*
     IF    FUR-F17 = SPACE
           GO TO                  READ-FURIKAF-SEC
     END-IF.
*
*****IF    FUR-F09           <    19930601
*          GO TO                  READ-FURIKAF-SEC
*****END-IF.
*
     ADD   1                 TO   READ-CNT.
 READ-FURIKAF-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*---((商品在庫マスタの検索キーの設定を行う))---*
*****IF     FUR-F16 = "3"
*           DISPLAY "FUR-F16 = " FUR-F16  UPON CONS
*****END-IF.
     MOVE   FUR-F17          TO   ZAI-F01.
     MOVE   FUR-F10          TO   ZAI-F021.
     MOVE   FUR-F11          TO   ZAI-F022.
*20091006 商品変換テーブルを索引 NAV ST
*20100129 サカタ様の指示により、_番の取得はなし ST ****
*****IF     FUR-F17  NOT =  SPACE
*           PERFORM   SHOTBL12-READ-SEC
*           IF  SHOTBL12-INV-FLG = "INV"
*               MOVE SPACE   TO   ZAI-F03  WK-ZAI03
*           ELSE
*               MOVE TBL-F08 TO   ZAI-F03  WK-ZAI03
*           END-IF
*    ELSE
*           MOVE    SPACE    TO   ZAI-F03  WK-ZAI03
*****END-IF.
     MOVE    SPACE    TO   ZAI-F03  WK-ZAI03.
*20100129 サカタ様の指示により、_番の取得はなし END****
*****DISPLAY "ZAI-F021 = " ZAI-F021 ":ZAI-F03 = " ZAI-F03
*****"FUR-F17 = " FUR-F17 UPON CONS.
*20091006 商品変換テーブルを索引 NAV ED
     READ    ZAMZAIF
         INVALID   KEY
             PERFORM    WRITE-ZAMZAIF-SEC
             GO              TO   MAIN-010
     END-READ.
     PERFORM    REWRITE-ZAMZAIF-SEC.
 MAIN-010.
*20091006 更新区分の更新 NAV ST
*****社内発注_項目に更新した_番をセット
*20100129 _番の更新はサカタ様の指示により停止  ST
*    MOVE       WK-ZAI03     TO   FUR-F18.
*20100129 _番の更新はサカタ様の指示により停止  END
     REWRITE    FUR-REC.
*20091006 更新区分の更新 NAV ED
     PERFORM    READ-FURIKAF-SEC.
 MAIN-END.
     EXIT.
****************************************************************
*      _１　　商品在庫マスタ追加処理                          *
****************************************************************
 WRITE-ZAMZAIF-SEC           SECTION.
*---((商品在庫マスタの検索キーの設定を行う))---*
     INITIALIZE                   ZAI-REC.
     MOVE   FUR-F17          TO   ZAI-F01.
     MOVE   FUR-F10          TO   ZAI-F021.
     MOVE   FUR-F11          TO   ZAI-F022.
*****MOVE   SPACE            TO   ZAI-F03.
     MOVE   WK-ZAI03         TO   ZAI-F03.
     PERFORM    EDIT-ZAMZAIF-SEC.
*商品名称マスタＲＥＡＤ（カナ名取得）
     MOVE   FUR-F10          TO   MEI-F011.
     MOVE   FUR-F11          TO   MEI-F012.
     READ   MEIMS1
       INVALID       KEY
            MOVE  FUR-REC    TO   FUE-REC
            WRITE                 FUE-REC
            ADD      1       TO   ERR-CNT
            DISPLAY "商品マスタ未登録　"
            "コード　＝　"   FUR-F11
            "  品単　＝　"   FUR-F12
            UPON CONS
            MOVE "1"         TO   FUR-F29
************2009/10/08 追加
            MOVE SPACE       TO   FUR-F20
            GO               TO   WRITE-ZAMZAIF-END
       NOT  INVALID  KEY
            MOVE  MEI-F031   TO   ZAI-F30
            MOVE  SPACE      TO   FUR-F29
************2009/10/08 追加
            MOVE  MEI-F91    TO   FUR-F20
     END-READ.
*
     MOVE     SYS-DATE-YMD   TO   ZAI-F98
     MOVE     SYS-DATE-YMD   TO   ZAI-F99
     WRITE    ZAI-REC.
     ADD   1                 TO   WRITE-CNT.
 WRITE-ZAMZAIF-END.
     EXIT.
****************************************************************
*      __１　商品在庫マスタ編集処理                          *
****************************************************************
 EDIT-ZAMZAIF-SEC            SECTION.
*
     MOVE    FUR-F09(1:6)     TO    WRK-DATE2.
*符号解除
     MOVE    FUR-F13          TO    WK-FUR-F13.
*
     IF   WRK-DATE2    =    WRK-DATE1
          EVALUATE   FUR-F16
              WHEN  "1"
*                        現在庫数
                   COMPUTE   ZAI-F04   =   ZAI-F04  +  WK-FUR-F13
                   COMPUTE   ZAI-F06   =   ZAI-F06  +  WK-FUR-F13
*                        当月入庫数
                   COMPUTE   ZAI-F07   =   ZAI-F07  +  WK-FUR-F13
*20091006 NAV ST
*##                IF  FUR-F02 = "70" OR "71"
*                      COMPUTE ZAI-F18 =   ZAI-F18  -  WK-FUR-F13
*                      COMPUTE ZAI-F19 =   ZAI-F19  +  WK-FUR-F13
*##                END-IF
*20091006 NAV ED
              WHEN  "2"
                   COMPUTE   ZAI-F04   =   ZAI-F04  -  WK-FUR-F13
                   COMPUTE   ZAI-F06   =   ZAI-F06  -  WK-FUR-F13
*                        当月出庫数
                   COMPUTE   ZAI-F08   =   ZAI-F08  +  WK-FUR-F13
*20091006 NAV ST
*##                IF  FUR-F02 = "70" OR "71"
*                      COMPUTE ZAI-F18 =   ZAI-F18  +  WK-FUR-F13
*                      COMPUTE ZAI-F19 =   ZAI-F19  -  WK-FUR-F13
*##                END-IF
*20091006 NAV ED
              WHEN  "3"
*******************CONTINUE
*20091006 NAV ST
                   COMPUTE   ZAI-F18   =   ZAI-F18  +  FUR-F13
*20091006 NAV ED
              WHEN   OTHER
                   DISPLAY "入出庫区分エラー　伝票_ "
                           FUR-F03 " 行_ " FUR-F04  UPON CONS
          END-EVALUATE
     ELSE
*次月分データ処理
       IF WRK-DATE2    >    WRK-DATE1
          EVALUATE   FUR-F16
              WHEN  "1"
*                        次月入庫数
                   COMPUTE   ZAI-F11   =   ZAI-F11  +  WK-FUR-F13
                   COMPUTE   ZAI-F04   =   ZAI-F04  +  WK-FUR-F13
*20091006 NAV ST
*##                IF  FUR-F02 = "70" OR "71"
*                      COMPUTE ZAI-F18 =   ZAI-F18  -  WK-FUR-F13
*                      COMPUTE ZAI-F19 =   ZAI-F19  +  WK-FUR-F13
*##                END-IF
*20091006 NAV ED
              WHEN  "2"
*                        次月出庫数
                   COMPUTE   ZAI-F12   =   ZAI-F12  +  WK-FUR-F13
                   COMPUTE   ZAI-F04   =   ZAI-F04  -  WK-FUR-F13
*20091006 NAV ST
*##                IF  FUR-F02 = "70" OR "71"
*                      COMPUTE ZAI-F18 =   ZAI-F18  +  WK-FUR-F13
*                      COMPUTE ZAI-F19 =   ZAI-F19  -  WK-FUR-F13
*##                END-IF
*20091006 NAV ED
              WHEN  "3"
*******************CONTINUE
*20091006 NAV ST
                   COMPUTE   ZAI-F18   =   ZAI-F18  +  FUR-F13
*20091006 NAV ED
              WHEN   OTHER
                   DISPLAY "入出庫区分エラー　伝票_ "
                           FUR-F03 " 行_ " FUR-F04  UPON CONS
          END-EVALUATE
       ELSE
*前月分データ処理
          EVALUATE   FUR-F16
              WHEN  "1"
*                        前月入庫数
                   COMPUTE   ZAI-F15   =   ZAI-F15  +  WK-FUR-F13
                   COMPUTE   ZAI-F04   =   ZAI-F04  +  WK-FUR-F13
                   COMPUTE   ZAI-F05   =   ZAI-F05  +  WK-FUR-F13
*20091006 NAV ST
*##                IF  FUR-F02 = "70" OR "71"
*                      COMPUTE ZAI-F18 =   ZAI-F18  -  WK-FUR-F13
*                      COMPUTE ZAI-F19 =   ZAI-F19  +  WK-FUR-F13
*##                END-IF
*20091006 NAV ED
              WHEN  "2"
*                        前月出庫数
                   COMPUTE   ZAI-F16   =   ZAI-F16  +  WK-FUR-F13
                   COMPUTE   ZAI-F04   =   ZAI-F04  -  WK-FUR-F13
*20091006 NAV ST
*##                IF  FUR-F02 = "70" OR "71"
*                      COMPUTE ZAI-F18 =   ZAI-F18  +  WK-FUR-F13
*                      COMPUTE ZAI-F19 =   ZAI-F19  -  WK-FUR-F13
*##                END-IF
*20091006 NAV ED
              WHEN  "3"
*******************CONTINUE
*20091006 NAV ST
                   COMPUTE   ZAI-F18   =   ZAI-F18  +  FUR-F13
*20091006 NAV ED
              WHEN   OTHER
                   DISPLAY "入出庫区分エラー　伝票_ "
                           FUR-F03 " 行_ " FUR-F04  UPON CONS
          END-EVALUATE
        END-IF
     END-IF.
 EDIT-ZAMZAIF-END.
     EXIT.
****************************************************************
*      _２　　商品在庫マスタ更新処理                          *
****************************************************************
 REWRITE-ZAMZAIF-SEC         SECTION.
     PERFORM  EDIT-ZAMZAIF-SEC.
     MOVE     SYS-DATE-YMD   TO   ZAI-F99
     MOVE     SPACE          TO   FUR-F29
*商品名称マスタＲＥＡＤ（カナ名取得）
     MOVE   FUR-F10          TO   MEI-F011.
     MOVE   FUR-F11          TO   MEI-F012.
     READ   MEIMS1
       INVALID       KEY
************2009/10/08 追加
            MOVE  SPACE      TO   FUR-F20
       NOT  INVALID  KEY
************2009/10/08 追加
            MOVE  MEI-F91    TO   FUR-F20
     END-READ.
     REWRITE  ZAI-REC.
     ADD   1                 TO   REWRITE-CNT.
 REWRITE-ZAMZAIF-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              FURIKAF  HJYOKEN  FURIKAE
                        ZAMZAIF  MEIMS1   SHOTBL12.
     DISPLAY  "FURIKAF    (IN) = "  READ-CNT      UPON   CONS.
     DISPLAY  "ｼﾖｳﾋﾝｻﾞｲｺM (UP) = "  REWRITE-CNT   UPON   CONS.
     DISPLAY  "ｼﾖｳﾋﾝｻﾞｲｺM(ADD) = "  WRITE-CNT     UPON   CONS.
     DISPLAY  "ｴﾗｰ       (   ) = "  ERR-CNT       UPON   CONS.
 END-END.
     EXIT.
****************************************************************
*      ALL        商品変換テーブルマスタ索引                   *
****************************************************************
 SHOTBL12-READ-SEC      SECTION.
*
     MOVE     FUR-F17   TO    TBL-F04.
     MOVE     FUR-F10   TO    TBL-F031.
     MOVE     FUR-F11   TO    TBL-F032.
     READ     SHOTBL12
              INVALID     MOVE  "INV"    TO  SHOTBL12-INV-FLG
              NOT INVALID MOVE  SPACE    TO  SHOTBL12-INV-FLG
     END-READ.
*
 SHOTBL12-READ-EXIT.
     EXIT.
******************<<  PROGRAM  END  >>**************************
