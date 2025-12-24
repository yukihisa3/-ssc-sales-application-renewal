# SKY1302L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKY1302L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　タックシール出力　　　　　　　　　*
*    作成日／作成者　　　：　99/10/22  /HAGIWARA               *
*    更新日／更新者　　　：　99/12/28  /HAGIWARA               *
*                        　　店舗コード，_追加　　　　　　　　*
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　  *
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SKY1302L.
 AUTHOR.             NAV.
 DATE-WRITTEN.       99.10.22.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS     CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*----<< 画面　Ｆ >>----*
     SELECT  DSPF            ASSIGN    TO          GS-DSPF
                             SYMBOLIC  DESTINATION IS   "DSP"
                             PROCESSING  MODE      IS   DSP-PRO
                             GROUP                 IS   DSP-GRP
                             FORMAT                IS   DSP-FRM
                             SELECTED  FUNCTION    IS   DSP-FUNC
                             FILE      STATUS      IS   DSP-STA.
*----<<  送り状ファイル  >>-----*
     SELECT   ZOKURIF   ASSIGN    TO        ZOKURIF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   OKU-STA.
*----<< プリンタＦ >>-----*
     SELECT   PRTF           ASSIGN      TO  GS-PRTF
                             ORGANIZATION         IS   SEQUENTIAL
                             ACCESS MODE          IS   SEQUENTIAL
                             SYMBOLIC DESTINATION IS  "PRT"
                             PROCESSING MODE      IS   PRT-PRO
                             GROUP                IS   PRT-GRP
                             FORMAT               IS   PRT-FMT
                             SELECTED FUNCTION    IS   PRT-FNC
                             UNIT     CONTROL     IS   PRT-CTL
                             FILE STATUS          IS   PRT-STA
                             DESTINATION-1        IS   PRT-DES.
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*----<< 画面　Ｆ >>----*
 FD  DSPF.
     COPY    FKY13021  OF   XMDLIB
     JOINING  DSP      AS   PREFIX.
*----<< 送状ファイル  >>---*
 FD  ZOKURIF.
     COPY     ZOKURIF  OF  XFDLIB
     JOINING  OKU      AS  PREFIX.
*----<< プリンタＦ >>-----*
 FD  PRTF.
     COPY     FKY13022 OF  XMDLIB
     JOINING  PRT      AS  PREFIX.
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*画面Ｆ制御用
 01  DSP-CNTL                    VALUE    SPACE.
     03  DSP-FRM                 PIC  X(8).
     03  DSP-GRP                 PIC  X(8).
     03  DSP-PRO                 PIC  X(2).
     03  DSP-FUNC                PIC  X(4).
     03  DSP-STA                 PIC  X(2).
*プリンタＦ制御用
 01  PRT-CONTROL.
     03  PRT-PRO           PIC  X(02).
     03  PRT-GRP           PIC  X(08).
     03  PRT-FMT           PIC  X(08).
     03  PRT-DES           PIC  X(08).
     03  PRT-CTL           PIC  X(06).
     03  PRT-FNC           PIC  X(04).
*ステータス
 01  STA-AREA.
     03  OKU-STA             PIC  X(02).
     03  PRT-STA             PIC  X(02).
 01  WORK-AREA.
     03  MAISU               PIC  9(04)  VALUE  ZERO.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  W-KINGKX.
         05  W-YEN           PIC  N(01)  VALUE  NC"￥".
         05  W-KINGK         PIC  N(06)  VALUE  SPACE.
     03  W-SU                PIC  9(01).
     03  W-RSU     REDEFINES   W-SU.
      05  R-SU               PIC  X(01).
     03  I                   PIC  9(01)  VALUE  ZERO.
     03  J                   PIC  9(01)  VALUE  ZERO.
     03  IX                  PIC  9(01)  VALUE  ZERO.
*----------------- 1999/12/28追加 START -----------------------*
     03  WK-TENCD            PIC  9(05)  VALUE  ZERO.
*----------------- 1999/12/28追加 END   -----------------------*
*郵便番号編集
 01  YBNKIGOU                PIC  N(01)  VALUE  NC"〒".
 01  YUBIN.
     03  YUBIN1              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  X(01)  VALUE  "-".
     03  YUBIN2              PIC  X(04)  VALUE  SPACE.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  MSG-GAIDO.
     03  GAID-1                  PIC  N(30)     VALUE
         NC"_終了".
     03  GAID-2                  PIC  N(30)     VALUE
         NC"_取消　_終了　_項目戻り".
 01  MSG-WORLD.
     03  MSG-1                   PIC  N(20)     VALUE
         NC"１または２を入力してください。".
     03  MSG-2                   PIC  N(20)     VALUE
         NC"誤ったＰＦキーが押下されました。".
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SKY1302L".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
 PROCEDURE               DIVISION.
******************************************************************
 DECLARATIVES.
*プリントＦ
 DSP-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DSPF.
     MOVE    "DSPF"        TO    ERR-FL-ID.
     MOVE     DSP-STA      TO    ERR-STCD.
     DISPLAY  SEC-NAME     UPON  CONS.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*プリントＦ
 PRT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       PRTF.
     MOVE    "PRTF"        TO    ERR-FL-ID.
     MOVE     PRT-STA      TO    ERR-STCD.
     DISPLAY  SEC-NAME     UPON  CONS.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
*送状Ｆ
 JAN-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZOKURIF.
     MOVE    "ZOKURIF"      TO    ERR-FL-ID.
     MOVE     OKU-STA      TO    ERR-STCD.
     DISPLAY  SEC-NAME     UPON  CONS.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール                         0.0     *
*=============================================================
 CONTROL-SEC         SECTION.
     MOVE     "CONTROL-SEC"            TO   S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  INIT2-SEC.
     PERFORM  MAIN-SEC    UNTIL  END-FLG  =  "END"
     PERFORM  END-SEC.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理                             1.0     *
*=============================================================
 INIT-SEC            SECTION.
     MOVE     "INIT-SEC"               TO   S-NAME.
*
*****DISPLAY  "**  SKY1302L   START  **"   UPON  CONS.
*ファイル ＯＰＥＮ
     OPEN     I-O         DSPF.
     OPEN     INPUT       ZOKURIF.
     OPEN     OUTPUT      PRTF.
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
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
***  PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-SEC.
 INIT-EXIT.
     EXIT.
*=============================================================
*              処理区分入力処理                      1.1     *
*=============================================================
 DSP-SEC             SECTION.
     MOVE     "DSP-SEC"                TO   S-NAME.
*
*初期画面表示
 DSP-010.
     MOVE      LOW-VALUE         TO        DSP-FKY13021.
     MOVE      "FKY13021"        TO        DSP-FRM.
     MOVE      "CL"              TO        DSP-PRO.
*処理区分入力処理
 DSP-020.
     MOVE      GAID-1            TO        DSP-GAID.
     PERFORM   DSP-WRITE-SEC.
     MOVE      "KUBUN"           TO        DSP-GRP.
     MOVE      "NE"              TO        DSP-PRO.
     PERFORM   DSP-READ-SEC.
     EVALUATE  DSP-FUNC
         WHEN  "F005"
                MOVE    "END"    TO        END-FLG
                GO               TO        DSP-EXIT
         WHEN  "E000"
                IF   (DSP-KBN IS NOT NUMERIC) OR
                               (DSP-KBN NOT = 1 AND 2)
                      MOVE    MSG-1    TO  DSP-MSG
                      MOVE    "R"      TO  EDIT-OPTION OF DSP-KBN
                      MOVE    "C"      TO  EDIT-CURSOR OF DSP-KBN
                      GO               TO  DSP-020
                 ELSE
                      MOVE    SPACE    TO  DSP-MSG
                      MOVE    "D"      TO  EDIT-OPTION OF DSP-KBN
                      MOVE    " "      TO  EDIT-CURSOR OF DSP-KBN
                END-IF
         WHEN   OTHER
                MOVE    MSG-2    TO        DSP-MSG
                GO               TO        DSP-020
     END-EVALUATE.
*確認入力処理
 DSP-030.
     MOVE      GAID-2            TO        DSP-GAID.
     PERFORM   DSP-WRITE-SEC.
     MOVE      "TAIL"            TO        DSP-GRP.
     MOVE      "NE"              TO        DSP-PRO.
     PERFORM   DSP-READ-SEC.
     EVALUATE  DSP-FUNC
         WHEN  "F004"
                GO               TO        DSP-010
         WHEN  "F005"
                MOVE    "END"    TO        END-FLG
         WHEN  "F006"
                MOVE    SPACE    TO        DSP-MSG
                GO               TO        DSP-020
         WHEN  "E000"
                IF     DSP-KBN  =   1
                       PERFORM    TEST-SEC
                       GO        TO        DSP-010
                END-IF
     END-EVALUATE.
 DSP-EXIT.
     EXIT.
*=============================================================
*                テスト印刷                          1.1.1   *
*=============================================================
 TEST-SEC            SECTION.
     MOVE     "TEST-SEC"               TO   S-NAME.
*
     MOVE     SPACE          TO   PRT-MAS002.
*
     PERFORM  VARYING   IX   FROM  1  BY  1  UNTIL  IX > 4
              MOVE      ALL  "*"  TO PRT-YUBIN(IX)
              MOVE      NC"ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ"
                                  TO PRT-OKURI1(IX) PRT-OKURI2(IX)
                                     PRT-JUSHO1(IX) PRT-JUSHO2(IX)
              MOVE      NC"Ｎ"    TO PRT-YBN(IX)
              MOVE      NC"ＮＮ"  TO PRT-KEISH2(IX)
              MOVE      SPACE     TO PRT-KEISH1(IX)
*----------------- 1999/12/28追加 START -----------------------*
***             店舗コード
                  MOVE   "CD:"         TO   PRT-TXT1(IX)
                  MOVE   ALL "*"       TO   PRT-TENCD(IX)
***             電話番号
                  MOVE   "TEL:"        TO   PRT-TXT2(IX)
                  MOVE   ALL "*"       TO   PRT-TEL(IX)
*----------------- 1999/12/28追加 END   -----------------------*
     END-PERFORM.
*
     MOVE "FKY13022"              TO  PRT-FMT.
     MOVE  SPACE                  TO  PRT-CTL.
     MOVE  SPACE                  TO  PRT-PRO.
     MOVE "SCREEN"                TO  PRT-GRP.
     WRITE     PRT-FKY13022.
*
     CLOSE     PRTF.
     OPEN   OUTPUT   PRTF.
*
 TEST-EXIT.
     EXIT.
*=============================================================
*                ＰＲＴファイル初期処理              2.0
*=============================================================
 INIT2-SEC            SECTION.
     MOVE     "INIT2-SEC"              TO   S-NAME.
*
*送状ファイルＲＥＡＤ
     PERFORM   OKU-READ-SEC
     IF   END-FLG   =  "END"
          GO        TO   INIT2-EXIT
     END-IF.

     MOVE      ZERO      TO   MAISU.

 INIT2-EXIT.
     EXIT.
*=============================================================
*                メイン処理                          3.0
*=============================================================
 MAIN-SEC            SECTION.
     MOVE     "MAIN-SEC"               TO   S-NAME.
*
     PERFORM   VARYING  IX  FROM  1  BY  1  UNTIL  IX > 4
                                   OR     END-FLG  =  "END"
***             郵便番号
                  MOVE   OKU-F151      TO   YUBIN1
                  MOVE   OKU-F152      TO   YUBIN2
                  MOVE   YBNKIGOU      TO   PRT-YBN(IX)
                  MOVE   YUBIN         TO   PRT-YUBIN(IX)
***             送り先名称
                  MOVE   OKU-F05       TO   PRT-OKURI1(IX)
                  MOVE   OKU-F06       TO   PRT-OKURI2(IX)
***             住所
                  MOVE   OKU-F07       TO   PRT-JUSHO1(IX)
                  MOVE   OKU-F08       TO   PRT-JUSHO2(IX)
***             敬称
***               敬称区分＝１⇒御中　　
                  IF     OKU-F14       =    1
                         IF  OKU-F06   =    SPACE
                             MOVE  NC"御中"  TO  PRT-KEISH1(IX)
                             MOVE  SPACE     TO  PRT-KEISH2(IX)
                         ELSE
                             MOVE  SPACE     TO  PRT-KEISH1(IX)
                             MOVE  NC"御中"  TO  PRT-KEISH2(IX)
                         END-IF
                  ELSE
***               敬称区分＝２⇒様　　
                         IF  OKU-F14   =    2
                             IF  OKU-F06   =    SPACE
                                 MOVE  NC"様" TO PRT-KEISH1(IX)
                                 MOVE  SPACE  TO PRT-KEISH2(IX)
                             ELSE
                                 MOVE  SPACE  TO PRT-KEISH1(IX)
                                 MOVE  NC"様" TO PRT-KEISH2(IX)
                             END-IF
***               その他⇒空白　
                         ELSE
                             MOVE  SPACE    TO   PRT-KEISH1(IX)
                                                 PRT-KEISH2(IX)
                         END-IF
                  END-IF
                  ADD      1             TO      MAISU
*----------------- 1999/12/28追加 START -----------------------*
***             店舗コード
******************MOVE   OKU-F03       TO   WK-TENCD
                  MOVE   "CD:"         TO   PRT-TXT1(IX)
                  MOVE   OKU-F03       TO   PRT-TENCD(IX)
***             電話番号
                  MOVE   "TEL:"        TO   PRT-TXT2(IX)
                  MOVE   OKU-F09       TO   PRT-TEL(IX)
*----------------- 1999/12/28追加 END   -----------------------*
***  印刷枚数分を越えたら次のレコードを読み込む
            IF    MAISU  >=  OKU-F12  AND  END-FLG = SPACE
                  PERFORM    OKU-READ-SEC
                  IF     END-FLG  =  "END"
                         MOVE     9999      TO   MAISU
                  ELSE
                         MOVE     ZERO      TO   MAISU
                  END-IF
            END-IF
     END-PERFORM.
**出力
     MOVE "FKY13022"             TO  PRT-FMT.
     MOVE  SPACE                 TO  PRT-CTL.
     MOVE  SPACE                 TO  PRT-PRO.
     MOVE "SCREEN"               TO  PRT-GRP.
     WRITE     PRT-FKY13022.
     MOVE  SPACE                 TO  PRT-MAS002.
 MAIN-EXIT.
     EXIT.
*=============================================================
*                送状ファイルＲＥＡＤ処理                    *
*=============================================================
 OKU-READ-SEC      SECTION.
     MOVE     "OKU-READ-SEC"           TO   S-NAME.
*
     READ     ZOKURIF  AT  END
              MOVE   "END"    TO     END-FLG
     END-READ.
*
     IF       END-FLG  NOT = "END"
        AND   OKU-F12  =  ZERO
              GO       TO     OKU-READ-SEC
     END-IF.
*
 OKU-READ-EXIT.
     EXIT.
*=============================================================
*                画面ＦＲＥＡＤ処理                          *
*=============================================================
 DSP-READ-SEC      SECTION.
     MOVE     "DSP-READ-SEC"           TO   S-NAME.
*
     READ      DSPF.
     MOVE    SPACE        TO      DSP-PRO.
 DSP-READ-EXIT.
     EXIT.
*=============================================================
*                画面ＦＷＲＩＴＥ処理                        *
*=============================================================
 DSP-WRITE-SEC     SECTION.
     MOVE     "DSP-WRITE-SEC"          TO   S-NAME.
*
     MOVE      "SCREEN"          TO        DSP-GRP.
*日付／時刻セット
     MOVE      HEN-DATE           TO   DSP-SDATE.
     MOVE      HEN-TIME           TO   DSP-STIME.
*
     WRITE     DSP-FKY13021.
     MOVE    SPACE        TO      DSP-PRO.
 DSP-WRITE-EXIT.
     EXIT.
*=============================================================
*                 終了処理                           4.0     *
*=============================================================
 END-SEC             SECTION.
     MOVE     "END-SEC"                TO   S-NAME.
*
*ファイル ＣＬＯＳＥ
     CLOSE      DSPF    PRTF    ZOKURIF.
*****DISPLAY  "**  SKY1302L     END  **"   UPON  CONS.
 END-EXIT.
     EXIT.

```
