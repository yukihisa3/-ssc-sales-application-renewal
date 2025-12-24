****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿              *
*    業務名　　　　　　　：　本社－営業所間在庫管理　　　　　　*
*    モジュール名　　　　：　（営業所）在庫差分算出            *
*    作成日／作成者　　　：　2000/05/20 TAKAHASHI              *
*    再利用ＰＧ　　　　　：                                    *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SZA0030B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*商品在庫マスタ
     SELECT     ZAMZAIF       ASSIGN    TO       DA-01-VI-ZAMZAIF
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     SEQUENTIAL
                             RECORD    KEY      ZAI-F01
                                                ZAI-F02
                                                ZAI-F03
                             FILE      STATUS   ZAI-ST.
*商品在庫マスタ
     SELECT     BKZAIKO      ASSIGN    TO       DA-01-VI-BKZAIKO1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     SEQUENTIAL
                             RECORD    KEY      BZA-F01
                                                BZA-F02
                                                BZA-F03
                             FILE      STATUS   BZA-ST.
*在庫差分ファイル
     SELECT     SABZAIKO     ASSIGN    TO       DA-01-S-SABZAIKO
                             FILE      STATUS   SAB-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*商品在庫マスタ
 FD  ZAMZAIF.
     COPY     ZAMZAIF    OF        XFDLIB
              JOINING   ZAI       PREFIX.
*バックアップ商品在庫マスタ
 FD  BKZAIKO.
     COPY     ZAMZAIF    OF        XFDLIB
              JOINING   BZA       PREFIX.
*在庫差分ファイル
 FD  SABZAIKO           BLOCK  CONTAINS  5  RECORDS.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   SAB       PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
     03  BZA-ST              PIC  X(02)  VALUE  SPACE.
     03  SAB-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  ZAI-END             PIC  9(01)  VALUE  ZERO.
     03  BZA-END             PIC  9(01)  VALUE  ZERO.
     03  SAB-FLG             PIC  X(03)  VALUE  SPACE.
     03  OUT-CNT             PIC  9(07)  VALUE  ZERO.
     03  ZAI-CNT             PIC  9(07)  VALUE  ZERO.
     03  BZA-CNT             PIC  9(07)  VALUE  ZERO.
 01  WK-SYSTEM-DATE.
     03  SYS-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE-8          PIC  9(08)  VALUE  ZERO.
*商品在庫マスタ編集用
 01  WK-ZAI-REC.
     03  WK-ZAI-01           PIC  X(101).
     03  WK-ZAI-02           PIC  X(432).
     03  WK-ZAI-03           PIC  X(048).
     03  WK-ZAI-04           PIC  X(019).
*バックアップ商品在庫マスタ編集用
 01  WK-ZAIKO-REC.
     03  WK-ZAIKO-F01        PIC  X(101).
     03  WK-ZAIKO-F02        PIC  X(048).
     03  WK-ZAIKO-F03        PIC  X(051).
*商品在庫マスタキー
 01  ZAI-KEY.
     03  ZAI-KEY-01          PIC  9(02).
     03  ZAI-KEY-02          PIC  X(16).
     03  ZAI-KEY-03          PIC  X(06).
*バックアップ商品在庫マスタキー
 01  BZA-KEY.
     03  BZA-KEY-01          PIC  9(02).
     03  BZA-KEY-02          PIC  X(16).
     03  BZA-KEY-03          PIC  X(06).
*
 01  FILE-ERR.
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"在庫マスタ異常".
     03  BZA-ERR             PIC  N(10)  VALUE
                   NC"在庫マスタ異常ＢＫ".
     03  SAB-ERR             PIC  N(10)  VALUE
                   NC"在庫差分Ｆ異常".
*
 01  SEC-NAME.
     03  FILLER              PIC  X(16)  VALUE "## ｴﾗｰSECTION = ".
     03  S-NAME              PIC  X(20).
     03  FILLER              PIC  X(03)  VALUE " ##".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
****************************************************************
 PROCEDURE                   DIVISION.
****************************************************************
 DECLARATIVES.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIF.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 BZA-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE BKZAIKO.
     DISPLAY     BZA-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     BZA-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SAB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SABZAIKO.
     DISPLAY     SAB-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SAB-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL    ZAI-END = 1
                               AND    BZA-END = 1.
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN        INPUT       ZAMZAIF    BKZAIKO.
     OPEN        OUTPUT      SABZAIKO.
*    商品在庫マスタ読込み
     PERFORM  ZAMZAIF-READ-SEC.
*    バックアップ商品在庫マスタ読込み
     PERFORM  BKZAIKO-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*在庫マスタとバックアップ在庫マスタ比較
     EVALUATE  TRUE
         WHEN  BZA-KEY > ZAI-KEY
***************商品在庫マスタが存在しない場合
               MOVE SPACE       TO  WK-ZAI-REC
               INITIALIZE           WK-ZAI-REC
               MOVE ZAI-REC     TO  WK-ZAI-REC
               MOVE SPACE       TO  WK-ZAIKO-REC
               INITIALIZE           WK-ZAIKO-REC
               MOVE WK-ZAI-01   TO  WK-ZAIKO-F01
               MOVE WK-ZAI-03   TO  WK-ZAIKO-F02
               MOVE SPACE       TO  SAB-REC
               INITIALIZE           SAB-REC
               MOVE WK-ZAIKO-REC TO SAB-REC
               WRITE SAB-REC
               ADD  1           TO  OUT-CNT
               PERFORM ZAMZAIF-READ-SEC
         WHEN  BZA-KEY = ZAI-KEY
***************同一キーが存在した場合差分算出
               PERFORM SABUN-SEC
               PERFORM ZAMZAIF-READ-SEC
               PERFORM BKZAIKO-READ-SEC
         WHEN  BZA-KEY < ZAI-KEY
               PERFORM  BKZAIKO-READ-SEC
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*              在庫差分算出処理
****************************************************************
 SABUN-SEC                   SECTION.
     MOVE     "SABUN-SEC"         TO   S-NAME.
*在庫差分ファイル初期化
     MOVE      SPACE              TO   SAB-REC.
     INITIALIZE                        SAB-REC.
     MOVE      SPACE              TO   SAB-FLG.
*キー部分転送
*    ＜倉庫コード＞
     MOVE      ZAI-F01            TO   SAB-F01.
*    ＜商品ＣＤ＋品単ＣＤ＞
     MOVE      ZAI-F02            TO   SAB-F02.
*    ＜_番＞
     MOVE      ZAI-F03            TO   SAB-F03.
*    ＜カナ名称＞
     MOVE      ZAI-F04            TO   SAB-F04.
*    ＜取引先コード＞
     MOVE      ZAI-F05            TO   SAB-F05.
*    ＜現在庫数＞
     IF        ZAI-F04  NOT =  BZA-F04
               COMPUTE  SAB-F04 = BZA-F04 - ZAI-F04
               COMPUTE  SAB-F04 = SAB-F04 * (-1)
               MOVE     "SAB"     TO   SAB-FLG
     END-IF.
*    ＜当月入出庫数＞
     IF        ZAI-F06  NOT =  BZA-F06
               COMPUTE  SAB-F06 = BZA-F06 - ZAI-F06
               COMPUTE  SAB-F06 = SAB-F06 * (-1)
               MOVE     "SAB"     TO   SAB-FLG
     END-IF.
*    ＜当月発注数＞
     IF        ZAI-F20  NOT =  BZA-F20
               COMPUTE  SAB-F20 = BZA-F20 - ZAI-F20
               COMPUTE  SAB-F20 = SAB-F20 * (-1)
               MOVE     "SAB"     TO   SAB-FLG
     END-IF.
*    ＜当月入庫数＞
     IF        ZAI-F07  NOT =  BZA-F07
               COMPUTE  SAB-F07 = BZA-F07 - ZAI-F07
               COMPUTE  SAB-F07 = SAB-F07 * (-1)
               MOVE     "SAB"     TO   SAB-FLG
     END-IF.
*    ＜当月出庫数＞
     IF        ZAI-F08  NOT =  BZA-F08
               COMPUTE  SAB-F08 = BZA-F08 - ZAI-F08
               COMPUTE  SAB-F08 = SAB-F08 * (-1)
               MOVE     "SAB"     TO   SAB-FLG
     END-IF.
*    ＜未入庫数＞
     IF        ZAI-F26  NOT =  BZA-F26
               COMPUTE  SAB-F26 = BZA-F26 - ZAI-F26
               COMPUTE  SAB-F26 = SAB-F26 * (-1)
               MOVE     "SAB"     TO   SAB-FLG
     END-IF.
*    ＜未出庫数＞
     IF        ZAI-F27  NOT =  BZA-F27
               COMPUTE  SAB-F27 = BZA-F27 - ZAI-F27
               COMPUTE  SAB-F27 = SAB-F27 * (-1)
               MOVE     "SAB"     TO   SAB-FLG
     END-IF.
*    ＜引当済数＞
     IF        ZAI-F28  NOT =  BZA-F28
               COMPUTE  SAB-F28 = BZA-F28 - ZAI-F28
               COMPUTE  SAB-F28 = SAB-F28 * (-1)
               MOVE     "SAB"     TO   SAB-FLG
     END-IF.
*    ＜次月入庫数＞
     IF        ZAI-F11  NOT =  BZA-F11
               COMPUTE  SAB-F11 = BZA-F11 - ZAI-F11
               COMPUTE  SAB-F11 = SAB-F11 * (-1)
               MOVE     "SAB"     TO   SAB-FLG
     END-IF.
*    ＜次月出庫数＞
     IF        ZAI-F12  NOT =  BZA-F12
               COMPUTE  SAB-F12 = BZA-F12 - ZAI-F12
               COMPUTE  SAB-F12 = SAB-F12 * (-1)
               MOVE     "SAB"     TO   SAB-FLG
     END-IF.
*    ＜当月売上数＞
     IF        ZAI-F09  NOT =  BZA-F09
               COMPUTE  SAB-F09 = BZA-F09 - ZAI-F09
               COMPUTE  SAB-F09 = SAB-F09 * (-1)
               MOVE     "SAB"     TO   SAB-FLG
     END-IF.
*    ＜次月売上数＞
     IF        ZAI-F13  NOT =  BZA-F13
               COMPUTE  SAB-F13 = BZA-F13 - ZAI-F13
               COMPUTE  SAB-F13 = SAB-F13 * (-1)
               MOVE     "SAB"     TO   SAB-FLG
     END-IF.
*    ＜社内発注数＞
     IF        ZAI-F18  NOT =  BZA-F18
               COMPUTE  SAB-F18 = BZA-F18 - ZAI-F18
               COMPUTE  SAB-F18 = SAB-F18 * (-1)
               MOVE     "SAB"     TO   SAB-FLG
     END-IF.
*    ＜社内発注消込数＞
     IF        ZAI-F19  NOT =  BZA-F19
               COMPUTE  SAB-F19 = BZA-F19 - ZAI-F19
               COMPUTE  SAB-F19 = SAB-F19 * (-1)
               MOVE     "SAB"     TO   SAB-FLG
     END-IF.
*差分発生の場合、在庫差分ファイルを作成
     IF      SAB-FLG  =  "SAB"
             WRITE  SAB-REC
             ADD    1    TO     OUT-CNT
     END-IF.
*
 SABUN-EXIT.
     EXIT.
****************************************************************
*            商品在庫マスタ読込み
****************************************************************
 ZAMZAIF-READ-SEC             SECTION.
     MOVE     "ZAMZAIF-READ-SEC"   TO   S-NAME.
*
     IF    ZAI-END  =  1
           GO                     TO   ZAMZAIF-READ-EXIT
     END-IF.
*
     READ  ZAMZAIF  AT  END
               MOVE    HIGH-VALUE TO   ZAI-KEY
               MOVE    1          TO   ZAI-END
           NOT  AT  END
               ADD       1        TO   ZAI-CNT
               MOVE    ZAI-F01    TO   ZAI-KEY-01
               MOVE    ZAI-F02    TO   ZAI-KEY-02
               MOVE    ZAI-F03    TO   ZAI-KEY-03
     END-READ.
*
     IF        ZAI-CNT(5:3) = "000"
               DISPLAY "## ZAI-CNT = " ZAI-CNT " ##" UPON CONS
     END-IF.
*
 ZAMZAIF-READ-EXIT.
     EXIT.
****************************************************************
*            バックアップ商品在庫マスタ読込み
****************************************************************
 BKZAIKO-READ-SEC            SECTION.
     MOVE     "BKZAIKO-READ-SEC"  TO   S-NAME.
*
     IF    BZA-END  =  1
           GO                     TO   BKZAIKO-READ-EXIT
     END-IF.
*
     READ  BKZAIKO   AT  END
               MOVE    HIGH-VALUE TO   BZA-KEY
               MOVE    1          TO   BZA-END
           NOT  AT  END
               ADD       1        TO   BZA-CNT
               MOVE    BZA-F01    TO   BZA-KEY-01
               MOVE    BZA-F02    TO   BZA-KEY-02
               MOVE    BZA-F03    TO   BZA-KEY-03
     END-READ.
*
     IF        BZA-CNT(5:3) = "000"
               DISPLAY "## BZA-CNT = " BZA-CNT " ##" UPON CONS
     END-IF.
*
 BKZAIKO-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     CLOSE ZAMZAIF BKZAIKO SABZAIKO.
*
     DISPLAY "## ZAI-CNT = " ZAI-CNT " ##"  UPON  CONS.
     DISPLAY "## BZA-CNT = " BZA-CNT " ##"  UPON  CONS.
     DISPLAY "## OUT-CNT = " OUT-CNT " ##"  UPON  CONS.
*
 END-EXIT.
     EXIT.
