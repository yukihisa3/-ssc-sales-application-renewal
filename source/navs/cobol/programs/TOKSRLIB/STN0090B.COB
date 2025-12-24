****************************************************************
*    顧客名　　　　　　　：（株）サカタのタネ殿　　　　　　　　*
*    業務名　　　　　　　：　_卸ローカル運用　　　　　　　　　*
*    モジュール名　　　　：　倉庫_卸結果データ集計・チェック　*
*    作成日／作成者　　　：　2021/03/16 INOUE                  *
*    処理内容　　　　　　：　倉庫から転送された_卸データを　　*
*    　　　　　　　　　　　　集計しチェックする　　　　　　　　*
*    変更日／作成者　　　：　                                  *
*    変更内容　　　　　　：                                   *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 STN0090B.
*    　　　　　　　流用元：　TAB0010B
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
*----<< _卸結果データ >>----*
     SELECT      SKTANAL2    ASSIGN    TO       DA-01-VI-SKTANAL2
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     SEQUENTIAL
                             RECORD    KEY      SKT-F01
                                                SKT-F02
                                                SKT-F06
                                                SKT-F07
                             FILE      STATUS   SKT-ST.
*----<< _卸結果集計データ >>----*
     SELECT      GKTANAL1    ASSIGN    TO       DA-01-VI-GKTANAL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     DYNAMIC
                             RECORD    KEY      GKT-F01
                                                GKT-F02
                                                GKT-F03
                                                GKT-F04
                             FILE      STATUS   GKT-ST.
*----<< 倉庫マスタ  >>----*
     SELECT      ZSOKMS1     ASSIGN    TO       DA-01-VI-ZSOKMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SOK-F01
                             FILE      STATUS   SOK-ST.
*---<<  条件ファイル　  >>---*
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-ST.
*
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*_卸結果データ
 FD  SKTANAL2
     LABEL       RECORD      IS        STANDARD.
     COPY        SKTANAL2    OF        XFDLIB
     JOINING     SKT         AS        PREFIX.
*_卸結果集計データ
 FD  GKTANAL1.
     COPY        GKTANAL1    OF        XFDLIB
     JOINING     GKT         AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS1.
     COPY        ZSOKMS1     OF        XFDLIB
     JOINING     SOK         AS        PREFIX.
*---<<  条件ファイル　  >>---*
 FD  JYOKEN1.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  SKT-ST              PIC  X(02)  VALUE  SPACE.
     03  GKT-ST              PIC  X(02)  VALUE  SPACE.
     03  SOK-ST              PIC  X(02)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  SKT-ENDFLG          PIC  X(01)  VALUE  SPACE.
     03  SKT-CNT             PIC  9(07)  VALUE  ZERO.
     03  GKT-CNT             PIC  9(07)  VALUE  ZERO.
     03  ERR-CNT             PIC  9(07)  VALUE  ZERO.
     03  UNKNOWN             PIC  9(07)  VALUE  ZERO.
     03  OK-CNT              PIC  9(07)  VALUE  ZERO.
**
     03  WK-ERR-TBL.
         05  ERR-KBN         PIC  X(01)  OCCURS  10.
     03  WK-ERR-KBN          PIC  X(01).
 01  BK-AREA.
     03  BK-SKT-F01         PIC  9(08)  VALUE  ZERO.
     03  BK-SKT-F02         PIC  X(02)  VALUE  SPACE.
     03  BK-SKT-F03         PIC  X(02)  VALUE  SPACE.
     03  BK-SKT-F04         PIC  X(02)  VALUE  SPACE.
     03  BK-SKT-F06         PIC  X(06)  VALUE  SPACE.
     03  BK-SKT-F07         PIC  X(13)  VALUE  SPACE.
     03  BK-SKT-F08         PIC  X(08)  VALUE  SPACE.
     03  BK-SKT-F09         PIC  X(08)  VALUE  SPACE.
     03  BK-SKT-F11         PIC  X(02)  VALUE  SPACE.
     03  BK-SKT-F12         PIC  X(02)  VALUE  SPACE.
     03  SUM-SKT-F10        PIC  9(06)  VALUE  ZERO.
*
 01  WK-SKT-F03              PIC  X(04)  VALUE SPACE.
 01  WK-SKT-F03-X            REDEFINES   WK-SKT-F03.
     03  WK-SKT-F03R         PIC  9(04).
*
 01  SOK-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  STA-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  SUB-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  JYO-INV-FLG             PIC  X(03)  VALUE  SPACE.
*
 01  WK-TANADATE             PIC  9(08)  VALUE  ZERO.
**
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
*
 01  SYS-DATE8               PIC  9(08)  VALUE  ZERO.
*
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
*
*
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
 01  FILE-ERR.
     03  SKT-ERR            PIC  N(10)  VALUE
                   NC"_卸結果データ異常".
     03  GKT-ERR             PIC  N(10)  VALUE
                   NC"_卸集計データ異常".
     03  SOK-ERR             PIC  N(10)  VALUE
                   NC"倉庫マスタ異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件ファイル異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "STN0090B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "STN0090B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  MSG-OUT1-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT1-FIL2   PIC  N(11)  VALUE
                             NC"_卸結果データ読込　＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  MSG-OUT1-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT1-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT2.
         05  MSG-OUT2-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT2-FIL2   PIC  N(11)  VALUE
                             NC"_卸集計データ作成　＝".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  MSG-OUT2-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT2-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT3.
         05  MSG-OUT3-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT3-FIL2   PIC  N(11)  VALUE
                             NC"　内．エラーデータ　＝".
         05  MSG-OUT03       PIC  ZZZ,ZZ9.
         05  MSG-OUT3-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT3-FIL4   PIC  N(01)  VALUE NC"件".
     03  MSG-OUT4.
         05  MSG-OUT4-FIL1   PIC  X(02)  VALUE "##".
         05  MSG-OUT4-FIL2   PIC  N(11)  VALUE
                             NC"　内．ＯＫデータ　　＝".
         05  MSG-OUT04       PIC  ZZZ,ZZ9.
         05  MSG-OUT4-FIL3   PIC  X(01)  VALUE " ".
         05  MSG-OUT4-FIL4   PIC  N(01)  VALUE NC"件".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
   LINKAGE                     SECTION.
****************************************************************
   01  LINK-OUT-CNT1               PIC  9(07).
   01  LINK-OUT-CNT2               PIC  9(07).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-OUT-CNT1
                                               LINK-OUT-CNT2.
****************************************************************
 DECLARATIVES.
 SKT-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SKTANAL2.
     DISPLAY     SKT-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SKT-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 GKT-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE GKTANAL1.
     DISPLAY     GKT-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     GKT-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZSOKMS1.
     DISPLAY     SOK-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SOK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JYOKEN1.
     DISPLAY     JYO-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JYO-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  =  "END".
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE       "INIT-SEC"   TO   S-NAME.
*
     DISPLAY     MSG-START   UPON  CONS.
*
     OPEN        INPUT       SKTANAL2 ZSOKMS1 JYOKEN1.
     OPEN        OUTPUT      GKTANAL1.
*
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     ACCEPT   SYS-TIME          FROM   TIME.
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
     MOVE      LINK-OUT-YMD       TO   WK-DATE8
                                       SYS-DATE8.
     DISPLAY "# HIDUKE = " WK-DATE8    UPON CONS.
     DISPLAY "# JIKAN  = " WK-TIME-HM  UPON CONS.
*
*_卸日取得
     MOVE   99               TO   JYO-F01.
     MOVE   "TANA"           TO   JYO-F02.
     READ    JYOKEN1
         INVALID
           MOVE   "END"      TO   END-FLG
           DISPLAY  "ｼﾞｳｹﾝﾌｱｲﾙ INVALID ]]"   UPON   CONS
           DISPLAY  " F01=99 F02=TANA    "   UPON   CONS
         NOT   INVALID
           MOVE   JYO-F04    TO   WK-TANADATE
     END-READ.
*
* 初期ＲＥＡＤ
     PERFORM  READ-SKT-SEC.
*
     IF       SKT-ENDFLG  NOT =   SPACE
              DISPLAY NC"対象データなし" UPON CONS
              MOVE    "END"        TO  END-FLG
              GO                   TO  INIT-EXIT
     END-IF.
*
* ブレイク前レコード項目保管
     MOVE     SKT-F01              TO  BK-SKT-F01.
     MOVE     SKT-F02              TO  BK-SKT-F02.
     MOVE     SKT-F03              TO  BK-SKT-F03.
     MOVE     SKT-F04              TO  BK-SKT-F04.
     MOVE     SKT-F06              TO  BK-SKT-F06.
     MOVE     SKT-F07              TO  BK-SKT-F07.
     MOVE     SKT-F08              TO  BK-SKT-F08.
     MOVE     SKT-F09              TO  BK-SKT-F09.
     MOVE     SKT-F11              TO  BK-SKT-F11.
     MOVE     SKT-F12              TO  BK-SKT-F12.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
 MAIN-100.
* ブレイク判定
     IF     ( SKT-F01  NOT =  BK-SKT-F01 ) OR
            ( SKT-F02  NOT =  BK-SKT-F02 ) OR
            ( SKT-F06  NOT =  BK-SKT-F06 ) OR
            ( SKT-F07  NOT =  BK-SKT-F07 ) OR
            ( END-FLG      =  "END"      )
*         ブレイク
*           レコード項目セット
              MOVE     SPACE         TO   GKT-REC
              INITIALIZE                  GKT-REC
              MOVE     BK-SKT-F01    TO   GKT-F01
              MOVE     BK-SKT-F02    TO   GKT-F02
              MOVE     BK-SKT-F06    TO   GKT-F03
              MOVE     BK-SKT-F07    TO   GKT-F04
              MOVE     BK-SKT-F08    TO   GKT-F05
              MOVE     BK-SKT-F09    TO   GKT-F06
              MOVE     SUM-SKT-F10   TO   GKT-F07
              MOVE     BK-SKT-F12    TO   GKT-F08
              MOVE     BK-SKT-F11    TO   GKT-F09
              MOVE     BK-SKT-F03    TO   GKT-F10
              MOVE     BK-SKT-F04    TO   GKT-F11
*           _卸日　常識チェック
              MOVE    "2"            TO   LINK-IN-KBN
              MOVE     GKT-F01       TO   LINK-IN-YMD8
              CALL    "SKYDTCKB"  USING   LINK-IN-KBN
                                          LINK-IN-YMD6
                                          LINK-IN-YMD8
                                          LINK-OUT-RET
                                          LINK-OUT-YMD
              IF  LINK-OUT-RET    =       ZERO
                  MOVE  SPACE     TO      GKT-F95
              ELSE
                  MOVE  "1"       TO      GKT-F95
              END-IF
              IF  GKT-F01         =       WK-TANADATE
                  MOVE  SPACE     TO      GKT-F95
              ELSE
                  MOVE  "1"       TO      GKT-F95
              END-IF
*           倉庫マスタ存在チェック
              MOVE     GKT-F02    TO      SOK-F01
              PERFORM  SOK-READ-SEC
              IF  SOK-INV-FLG     =      "   "
                  MOVE  SPACE     TO      GKT-F96
              ELSE
                  MOVE  "1"       TO      GKT-F96
              END-IF
*           SUB商品名称マスタ存在チェック
              IF  GKT-F05         NOT =   SPACE
                  MOVE  SPACE     TO      GKT-F97
              ELSE
                  MOVE  "1"       TO      GKT-F97
              END-IF
*           エラー有無判定
              IF  ( GKT-F95  =  SPACE ) AND
                  ( GKT-F96  =  SPACE ) AND
                  ( GKT-F97  =  SPACE ) AND
                  ( GKT-F98  =  SPACE ) AND
                  ( GKT-F99  =  SPACE )
                    MOVE   SPACE     TO   GKT-F94
                    ADD    1         TO   OK-CNT
              ELSE
                    MOVE  "1"        TO   GKT-F94
                    ADD    1         TO   ERR-CNT
              END-IF
*           集計データ出力
              WRITE    GKT-REC
              ADD      1             TO   GKT-CNT
*
*           ブレイクレコード項目置換
              IF       END-FLG    NOT =   "END"
                       MOVE     SKT-F01       TO   BK-SKT-F01
                       MOVE     SKT-F02       TO   BK-SKT-F02
                       MOVE     SKT-F03       TO   BK-SKT-F03
                       MOVE     SKT-F04       TO   BK-SKT-F04
                       MOVE     SKT-F06       TO   BK-SKT-F06
                       MOVE     SKT-F07       TO   BK-SKT-F07
                       MOVE     SKT-F08       TO   BK-SKT-F08
                       MOVE     SKT-F09       TO   BK-SKT-F09
                       MOVE     SKT-F11       TO   BK-SKT-F11
                       MOVE     SKT-F12       TO   BK-SKT-F12
                       MOVE     SKT-F10       TO   SUM-SKT-F10
              END-IF
     ELSE
*         非ブレイク
*           数量加算
              ADD      SKT-F10       TO   SUM-SKT-F10
     END-IF.
*
* 次ＲＥＡＤ
* 終了するまで繰りかえす
     IF       END-FLG     =   "END"
              GO          TO  MAIN-SEC-EXIT
     END-IF.
*
     PERFORM  READ-SKT-SEC.
*
     IF       SKT-ENDFLG   NOT =   SPACE
              MOVE   "END"    TO   END-FLG
     END-IF.
*
     GO                       TO   MAIN-100.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
* _卸結果データＲＥＡＤ
****************************************************************
 READ-SKT-SEC                SECTION.
     MOVE     "READ-SKT-SEC"      TO   S-NAME.
*
     READ     SKTANAL2   AT   END
              MOVE      "Y"       TO   SKT-ENDFLG
              GO                  TO   READ-SKT-EXIT
     END-READ.
*カウント（取込件数）
     ADD      1                   TO   SKT-CNT.
*
     IF  SKT-CNT(5:3) = "000" OR "500"
         DISPLAY "READ-CNT = " SKT-CNT  UPON  CONS
     END-IF.
*
 READ-SKT-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ検索
****************************************************************
 SOK-READ-SEC               SECTION.
     MOVE    "SOK-READ-SEC" TO   S-NAME.
*
     READ     ZSOKMS1
       INVALID
              MOVE "INV"     TO   SOK-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   SOK-INV-FLG
     END-READ.
 SOK-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     MOVE      SKT-CNT      TO    MSG-OUT01.
     MOVE      GKT-CNT      TO    MSG-OUT02.
     MOVE      ERR-CNT      TO    MSG-OUT03.
     MOVE      OK-CNT       TO    MSG-OUT04.
     DISPLAY   MSG-OUT1-FIL1 MSG-OUT1-FIL2 MSG-OUT01
               MSG-OUT1-FIL3 MSG-OUT1-FIL4 UPON CONS.
     DISPLAY   MSG-OUT2-FIL1 MSG-OUT2-FIL2 MSG-OUT02
               MSG-OUT2-FIL3 MSG-OUT2-FIL4 UPON CONS.
     DISPLAY   MSG-OUT3-FIL1 MSG-OUT3-FIL2 MSG-OUT03
               MSG-OUT3-FIL3 MSG-OUT3-FIL4 UPON CONS.
     DISPLAY   MSG-OUT4-FIL1 MSG-OUT4-FIL2 MSG-OUT04
               MSG-OUT4-FIL3 MSG-OUT4-FIL4 UPON CONS.
*
     CLOSE     SKTANAL2
               ZSOKMS1
               JYOKEN1
               GKTANAL1.
*
*ＯＵＴパラメタセット
* 取込件数
     MOVE      SKT-CNT      TO    LINK-OUT-CNT1.
* エラー件数
     MOVE      ERR-CNT      TO    LINK-OUT-CNT2.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.
