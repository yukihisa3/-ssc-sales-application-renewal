****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　日次振替　　　　　　　　　　　　　*
*    作成日／更新日　　　：　93/05/17                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＴＯＫＵファイルを読んで在庫マスタ*
*                            を更新，又は追加する．　　　　　　*
*                                                              *
*    94.08.02            :   富士通側締日以降に前月分データが  *
*                        :   渡された場合前月分データの更新を  *
*                        :   行う　　　　　　                  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0210B.
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
*---<<  ＴＯＫＵファイル  >>---*
     SELECT   TOKU      ASSIGN    TO        DA-01-S-TOKU
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   TOK-STATUS.
*
*---<<  ＴＯＫＵファイル（エラー分）---*
     SELECT   TOKUE     ASSIGN    TO        DA-01-S-TOKUE
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   TOE-STATUS.
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
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＴＯＫＵファイル  >>---*
 FD  TOKU.
     COPY     TOKU.
*---<<  ＴＯＫＵファイル  >>---*
 FD  TOKUE.
 01  TOE-REC.
     03  TOE-ALL                 PIC  X(256).
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
     COPY     HMEIMS    OF      XFDLIB
              JOINING   MEI     AS    PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  TOK-STATUS          PIC  X(02).
     02  TOE-STATUS          PIC  X(02).
     02  ZAI-STATUS          PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE ZERO.
     02  REWRITE-CNT         PIC  9(07)  VALUE ZERO.
     02  WRITE-CNT           PIC  9(07)  VALUE ZERO.
     02  ERR-CNT             PIC  9(07)  VALUE ZERO.
****  ＷＲＫ領域  *** 1999/12/27 NAV START
 01  WRK-AREA.
     02  WRK-DATE1           PIC  9(06).
     02  WRK-DATE1R          REDEFINES   WRK-DATE1.
         04  WRK-DATE1R1     PIC  9(04).
         04  WRK-DATE1R2     PIC  9(02).
     02  WRK-DATE2           PIC  9(06).
 01  DATE-AREA.
     03  WK-DATE             PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE            PIC  9(08)  VALUE  ZERO.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0210B".
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
                     PROCEDURE    TOKU.
     MOVE     "TOKU"         TO   ERR-FL-ID.
     MOVE     TOK-STATUS     TO   ERR-STCD.
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
                     PROCEDURE    TOKUE.
     MOVE     "TOKUE"        TO   ERR-FL-ID.
     MOVE     TOE-STATUS     TO   ERR-STCD.
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
     OPEN    INPUT           TOKU      HJYOKEN   MEIMS1
             I-O             ZAMZAIF
             OUTPUT          TOKUE.
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
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
*
     PERFORM    READ-TOKU-SEC.
 INIT-END.
     EXIT.
****************************************************************
*      _１　　ＴＯＫＵファイルＲＥＡＤ処理                    *
****************************************************************
 READ-TOKU-SEC               SECTION.
     READ    TOKU
         AT   END
           MOVE   "END"      TO   END-FLG
           GO                TO   READ-TOKU-END
     END-READ.
*
     IF    TO0-F10           <    19930601
           GO TO                  READ-TOKU-SEC
     END-IF.
*
*    経費振替／経費振替返品については
*    よみとばし
*
*****IF    TO0-F02           =     80 OR 81
*****      GO TO                  READ-TOKU-SEC
*****END-IF.
*
     ADD   1                 TO   READ-CNT.
 READ-TOKU-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*---((商品在庫マスタの検索キーの設定を行う))---*
     MOVE   TO0-F18          TO   ZAI-F01.
     MOVE   TO0-F11          TO   ZAI-F021.
     MOVE   TO0-F12          TO   ZAI-F022.
     MOVE   SPACE            TO   ZAI-F03.
     READ    ZAMZAIF
         INVALID   KEY
             PERFORM    WRITE-ZAMZAIF-SEC
             GO              TO   MAIN-010
     END-READ.
     PERFORM    REWRITE-ZAMZAIF-SEC.
 MAIN-010.
     PERFORM    READ-TOKU-SEC.
 MAIN-END.
     EXIT.
****************************************************************
*      _１　　商品在庫マスタ追加処理                          *
****************************************************************
 WRITE-ZAMZAIF-SEC            SECTION.
     MOVE   SPACE            TO   ZAI-REC.
     INITIALIZE                   ZAI-REC.
     MOVE   TO0-F18          TO   ZAI-F01.
     MOVE   TO0-F11          TO   ZAI-F021.
     MOVE   TO0-F12          TO   ZAI-F022.
     MOVE   SPACE            TO   ZAI-F03.
     PERFORM    EDIT-ZAMZAIF-SEC.
*商品名称マスタＲＥＡＤ（カナ名取得）
     MOVE   TO0-F11          TO   MEI-F011.
     MOVE   TO0-F12          TO   MEI-F012.
     READ   MEIMS1
       INVALID       KEY
            MOVE  TOK-REC    TO   TOE-REC
            WRITE                 TOE-REC
            ADD      1       TO   ERR-CNT
            DISPLAY "商品マスタ未登録　"
            "コード　＝　"   TO0-F11
            "  品単　＝　"   TO0-F12
            UPON CONS
            GO               TO   WRITE-ZAMZAIF-END
       NOT  INVALID  KEY
            MOVE  MEI-F031   TO   ZAI-F30
     END-READ.
     MOVE   SYS-DATE         TO   ZAI-F98.
     MOVE   SYS-DATE         TO   ZAI-F99.
*
     WRITE    ZAI-REC.
     ADD   1                 TO   WRITE-CNT.
 WRITE-ZAMZAIF-END.
     EXIT.
****************************************************************
*      __１　商品在庫マスタ編集処理                          *
****************************************************************
 EDIT-ZAMZAIF-SEC             SECTION.
*
     MOVE    TO0-F10(1:6)     TO    WRK-DATE2.
*
*条件ＦのＦ０５＜＝ＴＯＫＵ－Ｆ１０（出荷年月）
*当月分データの処理(94.08.02)
*
     IF   WRK-DATE2    =    WRK-DATE1
          EVALUATE   TO0-F19
              WHEN   1
*                        現在庫数
                   COMPUTE   ZAI-F04   =   ZAI-F04  +  TO0-F15
                   COMPUTE   ZAI-F06   =   ZAI-F06  +  TO0-F15
*                        当月入庫数
                   COMPUTE   ZAI-F07   =   ZAI-F07  +  TO0-F15
              WHEN   2
                   COMPUTE   ZAI-F04   =   ZAI-F04  -  TO0-F15
                   COMPUTE   ZAI-F06   =   ZAI-F06  -  TO0-F15
*                        当月出庫数
                   COMPUTE   ZAI-F08   =   ZAI-F08  +  TO0-F15
              WHEN   OTHER
                   DISPLAY "入出庫区分エラー　伝票_ "
                           TO0-F03 " 行_ " TO0-F04  UPON CONS
          END-EVALUATE
     ELSE
*次月分データ処理
       IF WRK-DATE2    >    WRK-DATE1
          EVALUATE   TO0-F19
              WHEN   1
*                        次月入庫数
                   COMPUTE   ZAI-F11   =   ZAI-F11  +  TO0-F15
                   COMPUTE   ZAI-F04   =   ZAI-F04  +  TO0-F15
***                COMPUTE   ZAI-FII   =   ZAI-FII  +  TO0-F15
              WHEN   2
*                        次月出庫数
                   COMPUTE   ZAI-F12   =   ZAI-F12  +  TO0-F15
                   COMPUTE   ZAI-F04   =   ZAI-F04  -  TO0-F15
***                COMPUTE   ZAI-FII   =   ZAI-FII  -  TO0-F15
              WHEN   OTHER
                   DISPLAY "入出庫区分エラー　伝票_ "
                           TO0-F03 " 行_ " TO0-F04  UPON CONS
          END-EVALUATE
       ELSE
*前月分データ処理
          EVALUATE   TO0-F19
              WHEN   1
*                        前月入庫数
                   COMPUTE   ZAI-F15   =   ZAI-F15  +  TO0-F15
                   COMPUTE   ZAI-F04   =   ZAI-F04  +  TO0-F15
                   COMPUTE   ZAI-F05   =   ZAI-F05  +  TO0-F15
              WHEN   2
*                        前月出庫数
                   COMPUTE   ZAI-F16   =   ZAI-F16  +  TO0-F15
                   COMPUTE   ZAI-F04   =   ZAI-F04  -  TO0-F15
                   COMPUTE   ZAI-F05   =   ZAI-F05  -  TO0-F15
              WHEN   OTHER
                   DISPLAY "入出庫区分エラー　伝票_ "
                           TO0-F03 " 行_ " TO0-F04  UPON CONS
          END-EVALUATE
        END-IF
     END-IF.
 EDIT-ZAMZAIF-END.
     EXIT.
****************************************************************
*      _２　　商品在庫マスタ更新処理                          *
****************************************************************
 REWRITE-ZAMZAIF-SEC          SECTION.
     PERFORM    EDIT-ZAMZAIF-SEC.
     MOVE       SYS-DATE     TO   ZAI-F99.
     REWRITE    ZAI-REC.
     ADD   1                 TO   REWRITE-CNT.
 REWRITE-ZAMZAIF-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              TOKU      HJYOKEN  TOKUE
                        ZAMZAIF    MEIMS1.
     DISPLAY  "TOKU       (IN) = "  READ-CNT      UPON   CONS.
     DISPLAY  "ｼﾖｳﾋﾝｻﾞｲｺM (UP) = "  REWRITE-CNT   UPON   CONS.
     DISPLAY  "ｼﾖｳﾋﾝｻﾞｲｺM(ADD) = "  WRITE-CNT     UPON   CONS.
     DISPLAY  "ｴﾗｰ       (   ) = "  ERR-CNT       UPON   CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
