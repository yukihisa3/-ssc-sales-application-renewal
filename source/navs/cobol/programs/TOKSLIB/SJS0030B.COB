****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿
*    業務名　　　　　　　：　実績管理システム
*    モジュール名　　　　：　実績集計ファイル作成
*    作成日／作成者　　　：　2000/06/27 YOSHIDA
*    再利用ＰＧ　　　　　：
*    更新日／更新者　　　：　
*    更新履歴　　　      ：　
*      2011/11/25 飯田/NAV 基幹サーバ統合、業務改善
*                          実績累積ファイルに項目を追加。
*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SJS0030B.
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
*累積集計ファイル
     SELECT     RUISEKF      ASSIGN    TO        DA-01-VI-RUISEKL2
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      SEQUENTIAL
                             RECORD    KEY       RUI-F21
                             FILE      STATUS    RUI-ST.
*実績集計ファイル
     SELECT     JISSYUF      ASSIGN    TO        DA-01-VI-JISSYUL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       JIS-F01
                                                 JIS-F021
                                                 JIS-F022
                                                 JIS-F03
                                                 JIS-F04
                                                 JIS-F051
                                                 JIS-F052
                             FILE      STATUS    JIS-ST.

* 2011/11/25,S  S.I/NAV
*実績週集計ファイル
     SELECT     JISSSYUF     ASSIGN    TO        JISSSYU1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY
                               JSS-F01  *> 売上仕入区分
                               JSS-F021 *> 年
                               JSS-F022 *> 週番号
                               JSS-F03  *> 得意先（仕入先）
                               JSS-F04  *> 倉庫ＣＤ
                               JSS-F051 *> 商品ＣＤ
                               JSS-F052 *> 品単ＣＤ
                               JSS-F12  *> 相手商品ＣＤ
                             FILE      STATUS    JSS-ST.
* 2011/11/25,E  S.I/NAV

****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*累積集計ファイル
 FD  RUISEKF.
     COPY     RUISEKF   OF        XFDLIB
              JOINING   RUI       PREFIX.
*実績集計ファイル
 FD  JISSYUF.
     COPY     JISSYUF   OF        XFDLIB
              JOINING   JIS       PREFIX.
* 2011/11/25,S  S.I/NAV
*実績週集計ファイル
 FD  JISSSYUF.
     COPY     JISSYUF   OF        XFDLIB
              JOINING   JSS       PREFIX.
* 2011/11/25,E  S.I/NAV
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)    VALUE  SPACE.
     03  RUI-ST              PIC  X(02)    VALUE  SPACE.
     03  JIS-ST              PIC  X(02)    VALUE  SPACE.
* 2011/11/25,S  S.I/NAV
     03  JSS-ST              PIC  X(02)    VALUE  SPACE.
* 2011/11/25,E  S.I/NAV
 01  WK-AREA.
     03  END-FLG             PIC  9(01)    VALUE  ZERO.
     03  OUT-CNT             PIC  9(07)    VALUE  ZERO.
     03  READ-CNT            PIC  9(07)    VALUE  ZERO.
     03  REWRITE-CNT         PIC  9(07)    VALUE  ZERO.
     03  WK-DATE             PIC  9(06)    VALUE  ZERO.
     03  WK-SYS-DATE         PIC  9(08)    VALUE  ZERO.
     03  WK-SUU              PIC S9(07)V99 VALUE  ZERO.
     03  WK-KIN              PIC S9(09)    VALUE  ZERO.
* 2011/11/25,S  S.I/NAV
     03  FG-JISSSYUF-INV     PIC  9(01).
     03  CT-JSS-WT           PIC  9(07)    VALUE  ZERO.
     03  CT-JSS-RWT          PIC  9(07)    VALUE  ZERO.
* 2011/11/25,E  S.I/NAV
*
 01  FILE-ERR.
     03  RUI-ERR             PIC  N(10)  VALUE
                   NC"累積集計ファイル異常".
     03  JIS-ERR             PIC  N(10)  VALUE
                   NC"実績集計ファイル異常".
* 2011/11/25,S  S.I/NAV
     03  JSS-ERR             PIC  N(10)  VALUE
                   NC"実績週集計Ｆ異常".
* 2011/11/25,E  S.I/NAV
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
 RUI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE RUISEKF.
     DISPLAY     RUI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     RUI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 JIS-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JISSYUF.
     DISPLAY     JIS-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JIS-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
* 2011/11/25,S  S.I/NAV
 JSS-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JISSSYUF.
     DISPLAY     JSS-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     JSS-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
* 2011/11/25,E  S.I/NAV
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  = 9.
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE  "INIT-SEC"            TO  S-NAME.

     OPEN  I-O  RUISEKF
                JISSYUF.
* 2011/11/25,S  S.I/NAV
     OPEN  I-O  JISSSYUF.
* 2011/11/25,E  S.I/NAV
*システム日付・時刻の取得
     ACCEPT  WK-DATE   FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING  LINK-IN-KBN
                              LINK-IN-YMD6
                              LINK-IN-YMD8
                              LINK-OUT-RET
                              LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  WK-SYS-DATE.

*読込
     PERFORM  RUISEKF-READ-SEC.

 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.

* 伝票区分、F02
*    40：売上伝票
*    41：売上返品
*    42：売上値引
*    44：売上割戻
*    45：売上値増
*    47：返品廃棄
*    50：仕入
*    51：返品
*    52：仕入値引
*    55：仕入値増
*    60：直送
*    61：直送返品
*    70：振替
*    71：振替返品
*    80：経費振替
*    81：経費振替返品
*    30：作業実績
*
     MOVE  SPACE            TO  JIS-REC.
     INITIALIZE  JIS-REC.
     IF  RUI-F02 =  "50" OR "51" OR "70" OR "71" OR
                    "35" OR "36"
         MOVE  "2"          TO  JIS-F01
     ELSE
         IF  RUI-F02 = "40" OR "41" OR "42" OR "45" OR
                       "60" OR "61"
             MOVE  "1"      TO  JIS-F01
         ELSE
             GO TO  MAIN-010
         END-IF
     END-IF.
     MOVE  RUI-F09(1:6)     TO  JIS-F02.
     MOVE  RUI-F06          TO  JIS-F03.
     MOVE  RUI-F17          TO  JIS-F04.
     MOVE  RUI-F10          TO  JIS-F051.
     MOVE  RUI-F11          TO  JIS-F052.
     READ  JISSYUF
       INVALID
*        実績集計ファイル、実績週集計ファイル、出力
         PERFORM   JISSYUF-WRITE-SEC
       NOT INVALID
*        実績集計ファイル、実績週集計ファイル、更新
         PERFORM   JISSYUF-REWRITE-SEC
     END-READ.

*    累積集計済み更新
     MOVE  "1"              TO  RUI-F21. *> 集計区分
     REWRITE  RUI-REC.

* 2011/11/25,S  S.I/NAV
     PERFORM  JISSSYUF-OT-SEC.
* 2011/11/25,E  S.I/NAV

 MAIN-010.
*    累積集計ファイル読込
     PERFORM  RUISEKF-READ-SEC.

 MAIN-EXIT.
     EXIT.
****************************************************************
*            累積集計ファイル読込
****************************************************************
 RUISEKF-READ-SEC        SECTION.
     MOVE  "RUISEKF-READ-SEC"    TO  S-NAME.

     READ  RUISEKF
       AT END
         MOVE  9            TO  END-FLG
         GO TO  RUISEKF-READ-EXIT
     END-READ.
*    集計済みのデータが来た時終了
     IF  RUI-F21 = "1"
         MOVE  9            TO  END-FLG
         GO TO  RUISEKF-READ-EXIT
     END-IF.
*読込件数
     ADD  1   TO  READ-CNT.

 RUISEKF-READ-EXIT.
     EXIT.
****************************************************************
*            実績集計ファイル出力
****************************************************************
 JISSYUF-WRITE-SEC           SECTION.
     MOVE  "JISSYUF-WRITE-SEC"   TO  S-NAME.

     MOVE  SPACE            TO  JIS-REC.
     INITIALIZE  JIS-REC.

     IF  RUI-F02 = "50" OR "51" OR "70" OR "71" OR
                   "35" OR "36"
         MOVE  "2"          TO  JIS-F01
     END-IF.

     IF  RUI-F02 = "40" OR "41" OR "42" OR "45" OR
                   "60" OR "61"
         MOVE  "1"          TO  JIS-F01
     END-IF.

     MOVE  RUI-F09(1:6)     TO  JIS-F02.
     MOVE  RUI-F06          TO  JIS-F03.
     MOVE  RUI-F17          TO  JIS-F04.
     MOVE  RUI-F10          TO  JIS-F051.
     MOVE  RUI-F11          TO  JIS-F052.

*    通常
     IF  RUI-F02 = "50" OR "70" OR "35" OR "40" OR "60"
         IF  RUI-F05 = "0" OR "2" OR "4" OR "6" OR "8"
             MOVE  RUI-F13  TO  JIS-F06
             MOVE  RUI-F15  TO  JIS-F07
         ELSE
             COMPUTE  JIS-F06 = RUI-F13 * -1
             COMPUTE  JIS-F07 = RUI-F15 * -1
         END-IF
     END-IF.

*    返品
     IF  RUI-F02 = "51" OR "71" OR "36" OR "41" OR "61"
         IF  RUI-F05 = "0" OR "2" OR "4" OR "6" OR "8"
             MOVE  RUI-F13  TO  JIS-F08
             MOVE  RUI-F15  TO  JIS-F09
         ELSE
             COMPUTE  JIS-F08 = RUI-F13 * -1
             COMPUTE  JIS-F09 = RUI-F15 * -1
         END-IF
     END-IF.

*    値引
     IF  RUI-F02 = "42"
         IF  RUI-F05 = "0" OR "2" OR "4" OR "6" OR "8"
         *> 2011/11/25,S  S.I/NAV
           *> 値引数量
             MOVE  RUI-F13  TO  JIS-F11
         *> 2011/11/25,E  S.I/NAV
             MOVE  RUI-F15  TO  JIS-F10
         ELSE
         *> 2011/11/25,S  S.I/NAV
           *> 値引数量
             COMPUTE  JIS-F11 = RUI-F13 * -1
         *> 2011/11/25,E  S.I/NAV
             COMPUTE  JIS-F10 = RUI-F15 * -1
         END-IF
     END-IF.

*    値増
     IF  RUI-F02 = "45"
         IF  RUI-F05 = "0" OR "2" OR "4" OR "6" OR "8"
             COMPUTE  JIS-F10 = RUI-F15 * -1
         ELSE
             MOVE  RUI-F15  TO JIS-F10
         END-IF
     END-IF.

* 2011/11/25,S  S.I/NAV
     MOVE  RUI-F23          TO  JIS-F12. *> 相手商品ＣＤ
     MOVE  RUI-F24          TO  JIS-F13. *> サカタ２０分類
     MOVE  RUI-F25          TO  JIS-F14. *> 商品分類区分
     MOVE  RUI-F28          TO  JIS-F15. *> 相手商品カナ名１
     MOVE  RUI-F29          TO  JIS-F16. *> 相手商品カナ名２

* 2011/11/25,E  S.I/NAV
     WRITE    JIS-REC.
*出力件数
     ADD      1         TO   OUT-CNT.
*
 JISSYUF-WRITE-EXIT.
     EXIT.
****************************************************************
*            実績集計ファイル更新
****************************************************************
 JISSYUF-REWRITE-SEC         SECTION.
     MOVE  "JISSYUF-REWRITE-SEC"  TO  S-NAME.

*    通常
     IF  RUI-F02 = "50" OR "70" OR "35" OR "40" OR "60"
         IF  RUI-F05 = "0" OR "2" OR "4" OR "6" OR "8"
             ADD  RUI-F13   TO  JIS-F06
             ADD  RUI-F15   TO  JIS-F07
         ELSE
             COMPUTE  WK-SUU = RUI-F13 * -1
             COMPUTE  WK-KIN = RUI-F15 * -1
             ADD  WK-SUU   TO  JIS-F06
             ADD  WK-KIN   TO  JIS-F07
         END-IF
     END-IF.

*    返品
     IF  RUI-F02 = "51" OR "71" OR "36" OR "41" OR "61"
         IF  RUI-F05 = "0" OR "2" OR "4" OR "6" OR "8"
             ADD  RUI-F13   TO  JIS-F08
             ADD  RUI-F15   TO  JIS-F09
         ELSE
             COMPUTE  WK-SUU = RUI-F13 * -1
             COMPUTE  WK-KIN = RUI-F15 * -1
             ADD  WK-SUU   TO  JIS-F08
             ADD  WK-KIN   TO  JIS-F09
         END-IF
     END-IF.

*    値引
     IF  RUI-F02 = "42"
         IF  RUI-F05 = "0" OR "2" OR "4" OR "6" OR "8"
        *> 2011/11/25,S  S.I/NAV
          *> 値引数量
             ADD  RUI-F13   TO  JIS-F11
        *> 2011/11/25,E  S.I/NAV
             ADD  RUI-F15   TO  JIS-F10
         ELSE
        *> 2011/11/25,S  S.I/NAV
          *> 値引数量
             COMPUTE  WK-SUU = RUI-F13 * -1
             ADD  WK-SUU   TO JIS-F11
        *> 2011/11/25,E  S.I/NAV
             COMPUTE  WK-KIN = RUI-F15 * -1
             ADD  WK-KIN   TO  JIS-F10
         END-IF
     END-IF.

*    値増
     IF  RUI-F02 = "45"
         IF  RUI-F05 = "0" OR "2" OR "4" OR "6" OR "8"
             COMPUTE  WK-KIN = RUI-F15  * -1
             ADD  WK-KIN   TO  JIS-F10
         ELSE
             ADD  RUI-F15  TO  JIS-F10
         END-IF
     END-IF.

     REWRITE  JIS-REC.
*出力件数
     ADD  1   TO  REWRITE-CNT.
*
 JISSYUF-REWRITE-EXIT.
     EXIT.
****************************************************************
*  累積週集計ファイル出力  2011/11/25 追加
****************************************************************
 JISSSYUF-OT-SEC        SECTION.
     MOVE  "JISSSYUF-OT-SEC"  TO  S-NAME.

     PERFORM  JISSSYUF-KEYEDT-SEC.

     READ  JISSSYUF
       INVALID
         MOVE  1            TO  FG-JISSSYUF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-JISSSYUF-INV
     END-READ.

     IF  FG-JISSSYUF-INV = 1 *> 新規作成
         INITIALIZE  JSS-REC
         PERFORM  JISSSYUF-KEYEDT-SEC
         PERFORM  JISSSYUF-EDT-SEC
         PERFORM  JISSSYUF-EDT2-SEC
         WRITE  JSS-REC
         ADD  1   TO  CT-JSS-WT
     ELSE                    *> 加算更新
         PERFORM  JISSSYUF-EDT2-SEC
         REWRITE  JSS-REC
         ADD  1   TO  CT-JSS-RWT
     END-IF.

 JISSSYUF-OT-EXIT.
     EXIT.
****************************************************************
*  累積週集計ファイルキー編集  2011/11/25 追加
****************************************************************
 JISSSYUF-KEYEDT-SEC        SECTION.
     MOVE  "JISSSYUF-KEYEDT-SEC"  TO  S-NAME.
  *> 売上仕入区分
     IF  RUI-F02 = "50" OR "51" OR "70" OR "71"
                OR "35" OR "36"
         MOVE  "2"          TO  JSS-F01 *> 仕入
     ELSE
         MOVE  "1"          TO  JSS-F01 *> 売上
     END-IF.

     MOVE  RUI-F09(1:4)     TO  JSS-F021. *> 年
     MOVE  RUI-F26          TO  JSS-F022. *> 週番号
     MOVE  RUI-F06          TO  JSS-F03.  *> 得意先（仕入先）
     MOVE  RUI-F17          TO  JSS-F04.  *> 倉庫ＣＤ
     MOVE  RUI-F10          TO  JSS-F051. *> 商品ＣＤ
     MOVE  RUI-F11          TO  JSS-F052. *> 品単ＣＤ
     MOVE  RUI-F23          TO  JSS-F12.  *> 相手商品ＣＤ
 JISSSYUF-KEYEDT-EXIT.
     EXIT.
****************************************************************
*  累積週集計ファイル編集  2011/11/25 追加
****************************************************************
 JISSSYUF-EDT-SEC        SECTION.
     MOVE  RUI-F24          TO  JSS-F13.  *> サカタ２０分類
     MOVE  RUI-F25          TO  JSS-F14.  *> 商品分類区分
     MOVE  RUI-F28          TO  JSS-F15.  *> 相手商品カナ名１
     MOVE  RUI-F29          TO  JSS-F16.  *> 相手商品カナ名２
 JISSSYUF-EDT-EXIT.
     EXIT.
****************************************************************
*  累積週集計ファイル編集２  2011/11/25 追加
****************************************************************
 JISSSYUF-EDT2-SEC        SECTION.
*    仕入、売上
     IF  RUI-F02 = "50" OR "70" OR "35" OR "40" OR "60"
         IF  RUI-F05 = "0" OR "2" OR "4" OR "6" OR "8" *> 黒伝
             COMPUTE  JSS-F06 = JSS-F06 + RUI-F13 *> 数量
             COMPUTE  JSS-F07 = JSS-F07 + RUI-F15 *> 金額
         ELSE                                          *> 赤伝
             COMPUTE  JSS-F06 =
                         JSS-F06 + (RUI-F13 * -1) *> 数量
             COMPUTE  JSS-F07 =
                         JSS-F07 + (RUI-F15 * -1) *> 金額
         END-IF
     END-IF.

*    仕入返品、売上返品
     IF  RUI-F02 = "51" OR "71" OR "36" OR "41" OR "61"
         IF  RUI-F05 = "0" OR "2" OR "4" OR "6" OR "8" *> 黒伝
             COMPUTE  JSS-F08 = JSS-F08 + RUI-F13 *> 数量
             COMPUTE  JSS-F09 = JSS-F09 + RUI-F15 *> 金額
         ELSE                                          *> 赤伝
             COMPUTE  JSS-F08 =
                         JSS-F08 + (RUI-F13 * -1) *> 数量
             COMPUTE  JSS-F09 =
                         JSS-F09 + (RUI-F15 * -1) *> 金額
         END-IF
     END-IF.

*    売上値引
     IF  RUI-F02 = "42"
         IF  RUI-F05 = "0" OR "2" OR "4" OR "6" OR "8" *> 黒伝
          *> 値引数量
             COMPUTE  JSS-F11 = JSS-F11 + RUI-F13 *> 数量
             COMPUTE  JSS-F10 = JSS-F10 + RUI-F15 *> 金額
         ELSE                                          *> 赤伝
             COMPUTE  JSS-F11 =
                         JSS-F11 + (RUI-F13 * -1) *> 数量
             COMPUTE  JSS-F10 =
                         JSS-F10 + (RUI-F15 * -1) *> 金額
         END-IF
     END-IF.

*    売上値増
     IF  RUI-F02 = "45"
         IF  RUI-F05 = "0" OR "2" OR "4" OR "6" OR "8" *> 黒伝
             COMPUTE  JSS-F10 =
                        JSS-F10 + (RUI-F15  * -1) *> 金額
         ELSE                                          *> 赤伝
             COMPUTE  JSS-F10 = JSS-F10 + RUI-F15 *> 金額
         END-IF
     END-IF.
 JISSSYUF-EDT2-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.

     CLOSE RUISEKF JISSYUF.
* 2011/11/25,S  S.I/NAV
     CLOSE JISSSYUF.
* 2011/11/25,E  S.I/NAV

* 2011/11/25,S  S.I/NAV
**     DISPLAY "## READ-CNT   = " READ-CNT     " ##"  UPON  CONS.
**     DISPLAY "## OUT-CNT    = " OUT-CNT      " ##"  UPON  CONS.
**     DISPLAY "## REWRITE-CNT= " REWRITE-CNT  " ##"  UPON  CONS.
     DISPLAY "## RUISEKF  READ-CNT    = " READ-CNT
             " ##"  UPON  CONS.
     DISPLAY "## JISSYUF  WRITE-CNT   = " OUT-CNT
             " ##"  UPON  CONS.
     DISPLAY "## JISSYUF  REWRITE-CNT = " REWRITE-CNT
             " ##"  UPON  CONS.
     DISPLAY "## JISSSYUF WRITE-CNT   = " CT-JSS-WT
             " ##"  UPON  CONS.
     DISPLAY "## JISSSYUF REWRITE-CNT = " CT-JSS-RWT
             " ##"  UPON  CONS.
* 2011/11/25,E  S.I/NAV
*
 END-EXIT.
     EXIT.
