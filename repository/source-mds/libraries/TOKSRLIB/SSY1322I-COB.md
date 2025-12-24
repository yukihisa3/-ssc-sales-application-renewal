# SSY1322I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY1322I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　コーナン　出荷　　　　　　　　　　*
*    モジュール名　　　　：　箱数入力（手書）　　　　　　　　　*
*    作成日／作成者　　　：　2021/09/14 INOUE                  *
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ピッキング時に記入した箱数を　　　*
*                            入力する。　　　　　　　　　　    *
*    更新日／更新者　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY1322I.
*                  流用:SSY1222I.
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
*---<<  画面ファイル  >>---*
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        DESTINATION-1       IS   DSP-WSNO
                        FILE      STATUS    IS   DSP-STATUS.
*
*---<<  箱数ファイル（手書）  >>---*
     SELECT   KTHAKOF   ASSIGN    TO        DA-01-VI-KTHAKOL2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       HAK-F001   HAK-F002
                                            HAK-F003   HAK-F004
                                            HAK-FA01   HAK-FA05
                                            HAK-FA02   HAK-FA03
                                            HAK-FA06   HAK-FA04
                        FILE      STATUS    IS   HAK-STATUS.
*---<<  取引先マスタ  >>---*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*---<<  倉庫マスタ  >>---*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*---<<  店舗マスタ  >>---*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52
                                                 TEN-F011
                        FILE      STATUS    IS   TEN-STATUS.
*---<<  条件ファイル  >>---*
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01  JYO-F02
                        FILE  STATUS   IS   JYO-STATUS.
*
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
 01  DSP-REC            PIC  X(2000).
     COPY     FSY13221  OF        XMDLIB.
*---<<  箱数ファイル（手書）  >>---*
 FD  KTHAKOF.
     COPY     KTHAKOF   OF        XFDLIB
              JOINING   HAK       PREFIX.
*---<<  取引先マスタ  >>---*
 FD  HTOKMS.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  倉庫マスタ  >>---*
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*---<<  店舗マスタ  >>---*
 FD  HTENMS.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*---<<  条件ファイル  >>---*
 FD  JYOKEN1.
     COPY     JYOKEN1   OF        XFDLIB
              JOINING   JYO       PREFIX.
*
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  画面制御項目  ****
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
     03  DSP-WSNO            PIC  X(08).
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  HAK-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
     02  SOK-STATUS          PIC  X(02).
     02  TEN-STATUS          PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  READ-FLG            PIC  X(01)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
     02  HAK-FLG             PIC  9(01)  VALUE ZERO.
     02  SOKKENF-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  ZSOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  HTOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  HTENMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  SOKKPGF-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  WK-CHK-BUMON        PIC  9(02)  VALUE ZERO.
     02  JYOKEN1-INV-FLG    PIC  X(03)  VALUE SPACE.
****  処理スイッチ  ****
 01  WK-AREA.
****  インデックス  ****
     02  IXA                 PIC  9(02).
     02  IXB                 PIC  9(02).
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
****  システム日付  ****
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
     03  SYS-DATE2                PIC  9(08).
*日付
 01  WK-NOUHIN.
     03  WK-NOUHIN1               PIC  9(02)  VALUE  ZERO.
     03  WK-NOUHIN2               PIC  9(06)  VALUE  ZERO.
 01  WK-HACYU.
     03  WK-HACYU1                PIC  9(02)  VALUE  ZERO.
     03  WK-HACYU2                PIC  9(06)  VALUE  ZERO.
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
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
*
 01  SAV-KEY.
     03  SAV-BKEY.
*      05  SAV-BKEY-KBN        PIC  X(01)   VALUE  ZERO.
*      05  SAV-BKEY-BUMON      PIC  9(03)   VALUE  ZERO.
       05  SAV-BKEY-ROUTE      PIC  X(02)   VALUE  ZERO.
       05  SAV-BKEY-BUMON      PIC  9(02)   VALUE  ZERO.
       05  SAV-BKEY-HDATE      PIC  9(06)   VALUE  ZERO.
     03  SAV-NKEY.
*      05  SAV-NKEY-KBN        PIC  X(01)   VALUE  ZERO.
*      05  SAV-NKEY-BUMON      PIC  9(03)   VALUE  ZERO.
       05  SAV-NKEY-ROUTE      PIC  X(02)   VALUE  ZERO.
       05  SAV-NKEY-BUMON      PIC  9(02)   VALUE  ZERO.
       05  SAV-NKEY-HDATE      PIC  9(06)   VALUE  ZERO.
*
****  商品右詰め用エリア      ****
 01  WK-SHOCD.
     03  WK-SHO              PIC  X(01)   OCCURS  8.
*
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG01            PIC N(07) VALUE
             NC"_取消　_終了".
     02  PMSG02            PIC N(19) VALUE
             NC"_取消　_終了　_戻り　_前頁　_次頁".
     02  PMSG03            PIC N(11) VALUE
             NC"_取消　_終了　_戻り".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY1322I".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージコード  ***
 01  CODE-AREA.
     02  ERR-MSG-CD          PIC  9(02)  VALUE  ZERO.
****  エラーメッセージ  ***
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(20)  VALUE
             NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(20)  VALUE
             NC"倉庫マスタに未登録です。".
     02  MSG-ERR3            PIC  N(20)  VALUE
             NC"倉庫ＣＤを入力して下さい。".
     02  MSG-ERR4            PIC  N(20)  VALUE
             NC"取引先マスタに未登録です。".
     02  MSG-ERR5            PIC  N(20)  VALUE
             NC"取引先ＣＤを入力して下さい。".
     02  MSG-ERR6            PIC  N(20)  VALUE
             NC"バッチ_を入力して下さい。".
     02  MSG-ERR7            PIC  N(20)  VALUE
             NC"店舗マスタに未登録です。".
     02  MSG-ERR8            PIC  N(20)  VALUE
             NC"バッチ_（日付）論理エラー".
     02  MSG-ERR9            PIC  N(20)  VALUE
             NC"バッチ_（時間）論理エラー".
     02  MSG-ERR10           PIC  N(20)  VALUE
             NC"前頁がありません。".
     02  MSG-ERR11           PIC  N(20)  VALUE
             NC"次頁がありません。".
     02  MSG-ERR12           PIC  N(20)  VALUE
             NC"店舗コードを入力して下さい。".
     02  MSG-ERR13           PIC  N(20)  VALUE
             NC"納品日を入力して下さい。".
     02  MSG-ERR14           PIC  N(20)  VALUE
             NC"納品日（日付）論理エラー".
     02  MSG-ERR15           PIC  N(20)  VALUE
             NC"該当データが存在しません。".
     02  MSG-ERR16           PIC  N(20)  VALUE
             NC"　　　　　　　　　　　　　".
     02  MSG-ERR17           PIC  N(20)  VALUE
             NC"　　　　　　　　　　　　".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(20)
                             OCCURS 17  TIMES.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
 LINKAGE                SECTION.
 01  PARA-BUMON            PIC X(04).
 01  PARA-TANTOU           PIC X(02).
 01  PARA-SOKCD            PIC X(02).
 01  PARA-DSOKCD           PIC X(02).
*----------------------------------------------------------*
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
*----------------------------------------------------------*
 PROCEDURE              DIVISION  USING  PARA-BUMON PARA-TANTOU
                                         PARA-SOKCD PARA-DSOKCD.
**
 DECLARATIVES.
 FILEERR-DSP            SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DSPF.
     MOVE     "DSPF    "       TO   ERR-FL-ID.
     MOVE      DSP-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-HAKE           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KTHAKOF.
     MOVE     "KTHAKOF"        TO   ERR-FL-ID.
     MOVE      HAK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-HTOK           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
     MOVE     "HTOKMS "        TO   ERR-FL-ID.
     MOVE      TOK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-ZSOK           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSOKMS.
     MOVE     "ZSOKMS "        TO   ERR-FL-ID.
     MOVE      SOK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-HTEN           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE     "HTENMS "        TO   ERR-FL-ID.
     MOVE      TEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-DCMH           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JYOKEN1.
     MOVE     "JYOKEN1"        TO   ERR-FL-ID.
     MOVE      JYO-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 END     DECLARATIVES.
************************************************************
 SSY1322I-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 SSY1322I-END.
     EXIT.
************************************************************
*      ■０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O   DSPF   KTHAKOF.
     OPEN     INPUT HTOKMS ZSOKMS HTENMS JYOKEN1.
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
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE2(1:4)     TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE2(5:2)     TO   HEN-DATE-MM.
     MOVE      SYS-DATE2(7:2)     TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     MOVE    "1"             TO   MAIN-FLG.
*
     MOVE     SPACE          TO   FSY13221.
*
     PERFORM       DSP-WRITE-SUB.
     PERFORM       EDIT-SET-HEAD.
     PERFORM       EDIT-SET-BODY.
*
 INIT-END.
     EXIT.
************************************************************
*      ■０      メイン処理                                *
************************************************************
 MAIN-SEC          SECTION.
     EVALUATE      MAIN-FLG
         WHEN      "1"  PERFORM   HEAD-SUB
         WHEN      "2"  PERFORM   BODY-SUB
         WHEN      "3"  PERFORM   KAKUNIN-SUB
         WHEN           OTHER     CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*==========================================================*
*      2.1       ヘッド部入力              MAIN-FLG=1      *
*==========================================================*
 HEAD-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG01         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE    "HEAD"          TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE     SPACE TO   FSY13221
                        PERFORM             HEADDEL-SUB
                        PERFORM             BODYDEL-SUB
         WHEN   "F005"
                        MOVE     "END" TO   END-FLG
         WHEN   "E000"
                        PERFORM   EDIT-SET-HEAD
                        PERFORM   BODYDEL-SUB
                        PERFORM   HEAD-CHK-SUB
                        IF   ERR-MSG-CD     =    ZERO
                             MOVE ZERO      TO   WK-CHK-BUMON
                             PERFORM   KTHAKOF-DSP-SUB
                             IF   ERR-MSG-CD     =    ZERO
                                  MOVE  BUMON(1) TO   WK-CHK-BUMON
                                  MOVE     "2"   TO   MAIN-FLG
                             END-IF
                        END-IF
         WHEN    OTHER
                        MOVE      01   TO   ERR-MSG-CD
     END-EVALUATE.
 HEAD-END.
     EXIT.
*==========================================================*
*      2.1.1     ヘッダ部チェック                          *
*==========================================================*
 HEAD-CHK-SUB         SECTION.
*
*バッチ■（日付）チェック
***  バッチ■（日付）未入力チェック
     IF       BDATE  NOT NUMERIC
         OR   BDATE  =  ZERO
              MOVE   06      TO   ERR-MSG-CD
              MOVE  "R"      TO   EDIT-OPTION  OF  BDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  BDATE
     ELSE
***           バッチ■（日付）論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     BDATE          TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  08       TO   ERR-MSG-CD
                   MOVE  "R"      TO   EDIT-OPTION  OF  BDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  BDATE
              ELSE
                   MOVE  "M"      TO   EDIT-OPTION  OF  BDATE
                   MOVE  SPACE    TO   EDIT-CURSOR  OF  BDATE
              END-IF
     END-IF.
*
*バッチ■（時間）チェック
***  バッチ■（時間）未入力チェック
     IF       BTIME     NOT NUMERIC
         OR   BTIME     =   ZERO
              MOVE  "R"      TO   EDIT-OPTION  OF  BTIME
              IF   ERR-MSG-CD     =    ZERO
                   MOVE   06      TO   ERR-MSG-CD
                   MOVE  "R"      TO   EDIT-OPTION  OF  BTIME
                   MOVE  "C"      TO   EDIT-CURSOR  OF  BTIME
              END-IF
     ELSE
***           バッチ■（時間）論理チェック
              MOVE     BTIME      TO   WK-JIKAN
              IF       WK-HH  <=  ZERO  OR  WK-HH  >  24 OR
                       WK-MM  <   ZERO  OR  WK-MM  >  59
                       IF     ERR-MSG-CD   =  ZERO
                              MOVE  09 TO  ERR-MSG-CD
                              MOVE "C" TO  EDIT-CURSOR OF BTIME
                       END-IF
                       MOVE "R"    TO EDIT-OPTION OF BTIME
              ELSE
                       MOVE "M"    TO EDIT-OPTION OF BTIME
                       MOVE SPACE  TO EDIT-CURSOR OF BTIME
              END-IF
     END-IF.
*
*バッチ■（取引先）チェック
***  バッチ■（取引先）未入力チェック
     IF       TOKCD    NOT NUMERIC
       OR     TOKCD    =  ZERO
         MOVE  "R"      TO   EDIT-OPTION  OF  TOKCD
         IF   ERR-MSG-CD     =    ZERO
              MOVE   05      TO   ERR-MSG-CD
              MOVE  "R"      TO   EDIT-OPTION  OF  TOKCD
              MOVE  "C"      TO   EDIT-CURSOR  OF  TOKCD
         END-IF
     ELSE
***           取引先ＲＥＡＤ
        MOVE       TOKCD          TO   TOK-F01
        PERFORM    HTOKMS-READ-SEC
        IF    HTOKMS-INV-FLG = SPACE
              MOVE     "M"        TO   EDIT-OPTION  OF  TOKCD
              MOVE      SPACE     TO   EDIT-CURSOR  OF  TOKCD
              MOVE      TOK-F03   TO   TOKNM
        ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE   04    TO   ERR-MSG-CD
                   MOVE  "C"    TO   EDIT-CURSOR  OF  TOKCD
                   MOVE   SPACE TO   TOKNM
              END-IF
              MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
        END-IF
     END-IF.
*
*    倉庫ＣＤチェック
     IF SOKCD   NOT =     SPACE
        MOVE    SOKCD    TO   SOK-F01
        PERFORM ZSOKMS-READ-SEC
        IF  ZSOKMS-INV-FLG = SPACE
            MOVE SOK-F02 TO   SOKNM
        ELSE
            IF   ERR-MSG-CD     =    ZERO
                 MOVE      02   TO   ERR-MSG-CD
                 MOVE  "C" TO   EDIT-CURSOR  OF  SOKCD
            END-IF
            MOVE   "R"     TO   EDIT-OPTION  OF  SOKCD
        END-IF
     ELSE
        IF   ERR-MSG-CD     =    ZERO
             MOVE      03   TO   ERR-MSG-CD
             MOVE  "C" TO   EDIT-CURSOR  OF  SOKCD
        END-IF
        MOVE   "R"     TO   EDIT-OPTION  OF  SOKCD
     END-IF.
***  店舗コード未入力チェック
     IF       CENTCD    NOT NUMERIC
         OR   CENTCD    =    ZERO
              IF   ERR-MSG-CD     =    ZERO
                   MOVE   12      TO   ERR-MSG-CD
                   MOVE  "C"      TO   EDIT-CURSOR  OF  CENTCD
              END-IF
              MOVE  "R"      TO   EDIT-OPTION  OF  CENTCD
     ELSE
***           店舗コードチェック
        MOVE       TOKCD          TO   TEN-F52
        MOVE       CENTCD         TO   TEN-F011
        PERFORM    HTENMS-READ-SEC
        IF    HTENMS-INV-FLG = SPACE
              MOVE     "M"        TO   EDIT-OPTION  OF  CENTCD
              MOVE      SPACE     TO   EDIT-CURSOR  OF  CENTCD
              MOVE      TEN-F02   TO   CENTNM
        ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE     07    TO   ERR-MSG-CD
                   MOVE    "C"    TO   EDIT-CURSOR  OF  CENTCD
                   MOVE     SPACE TO   CENTNM
              END-IF
              MOVE     "R"  TO    EDIT-OPTION  OF  CENTCD
        END-IF
     END-IF.
*
***  納品日未入力チェック
     IF       NOUHIN  NOT NUMERIC
         OR   NOUHIN  =  ZERO
              IF   ERR-MSG-CD     =    ZERO
                   MOVE    13     TO   ERR-MSG-CD
                   MOVE   "C"     TO   EDIT-CURSOR  OF  NOUHIN
              END-IF
              MOVE  "R"      TO   EDIT-OPTION  OF  NOUHIN
     ELSE
***           納品日（日付）論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     NOUHIN         TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   IF   ERR-MSG-CD  =  ZERO
                        MOVE  14    TO  ERR-MSG-CD
                        MOVE  "C"   TO  EDIT-CURSOR  OF  NOUHIN
                   END-IF
                   MOVE  "R"        TO  EDIT-OPTION  OF  NOUHIN
              ELSE
*
                   MOVE  "M"        TO  EDIT-OPTION  OF  NOUHIN
                   MOVE  SPACE      TO  EDIT-CURSOR  OF  NOUHIN
              END-IF
     END-IF.
*
 HEAD-CHK-EXIT.
     EXIT.
*==========================================================*
*      2.1.1     明細セット処理                            *
*==========================================================*
 KTHAKOF-DSP-SUB       SECTION.
*
     PERFORM  EDIT-SET-BODY.
     MOVE     ZERO      TO   ERR-MSG-CD.
*
     MOVE     ZERO      TO   HAK-FLG.
     PERFORM  KTHAKOF-START-SUB.
     IF       HAK-FLG   =    ZERO
              PERFORM   KTHAKOF-READ-SUB
              IF  HAK-FLG = 1
                  IF   ERR-MSG-CD     =    ZERO
                       MOVE     15    TO   ERR-MSG-CD
                  END-IF
                  MOVE   "C"     TO   EDIT-CURSOR  OF  BDATE
                  MOVE   "R"     TO   EDIT-OPTION  OF  BDATE
                  MOVE   "R"     TO   EDIT-OPTION  OF  BTIME
                  MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
                  MOVE   "R"     TO   EDIT-OPTION  OF  SOKCD
                  MOVE   "R"     TO   EDIT-OPTION  OF  CENTCD
                  MOVE   "R"     TO   EDIT-OPTION  OF  NOUHIN
                  GO      TO     KTHAKOF-DSP-EXIT
              END-IF
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE     15    TO   ERR-MSG-CD
              END-IF
              MOVE   "C"     TO   EDIT-CURSOR  OF  BDATE
              MOVE   "R"     TO   EDIT-OPTION  OF  BDATE
              MOVE   "R"     TO   EDIT-OPTION  OF  BTIME
              MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
              MOVE   "R"     TO   EDIT-OPTION  OF  SOKCD
              MOVE   "R"     TO   EDIT-OPTION  OF  CENTCD
              MOVE   "R"     TO   EDIT-OPTION  OF  NOUHIN
              GO      TO     KTHAKOF-DSP-EXIT
     END-IF.
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL   ( IXA       >    14 )  OR
                      ( HAK-FLG   =    1  )
*        ルート
              MOVE      HAK-FA03         TO   ROUTE(IXA)
*
*        部門ＣＤ
              MOVE      HAK-FA06         TO   BUMON(IXA)
*
*        部門名
              MOVE      28               TO   JYO-F01
              MOVE      HAK-FA06         TO   JYO-F02
              PERFORM   JYOKEN1-READ-SEC
              IF        JYOKEN1-INV-FLG  =  SPACE
                  MOVE   HAK-FA06           TO   BUMON(IXA)
                  MOVE   JYO-F03            TO   BUMONM(IXA)
              ELSE
                  MOVE   HAK-FA06           TO   BUMON(IXA)
                  MOVE   NC"＊＊＊＊＊＊"   TO   BUMONM(IXA)
              END-IF
*
*        発注日
         MOVE      "3"            TO       LINK-IN-KBN
         MOVE      HAK-FA04       TO       LINK-IN-YMD6
         MOVE      ZERO           TO       LINK-IN-YMD8
         MOVE      ZERO           TO       LINK-OUT-RET
         MOVE      ZERO           TO       LINK-OUT-YMD
         CALL      "SKYDTCKB"     USING    LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD
         MOVE      LINK-OUT-YMD   TO       HDATE(IXA)
*
*        箱数
         MOVE      HAK-FB01       TO       HAKOSU(IXA)
*
         IF        IXA  =    1
              MOVE HAK-FA03       TO       SAV-BKEY-ROUTE
              MOVE HAK-FA06       TO       SAV-BKEY-BUMON
              MOVE HAK-FA04       TO       SAV-BKEY-HDATE
         END-IF
         IF        IXA  =    14
              MOVE HAK-FA03       TO       SAV-NKEY-ROUTE
              MOVE HAK-FA06       TO       SAV-NKEY-BUMON
              MOVE HAK-FA04       TO       SAV-NKEY-HDATE
         END-IF
*
         PERFORM   KTHAKOF-READ-SUB
*
     END-PERFORM.
*
 KTHAKOF-DSP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.1.1.1   箱数ファイル　　　ＳＴＡＲＴ              *
*----------------------------------------------------------*
 KTHAKOF-START-SUB        SECTION.
*
     MOVE    ZERO            TO   HAK-FLG.
     MOVE    SPACE           TO   HAK-REC.
     INITIALIZE                   HAK-REC.
     MOVE    BDATE           TO   HAK-F001.
     MOVE    BTIME           TO   HAK-F002.
     MOVE    TOKCD           TO   HAK-F003.
     MOVE    SOKCD           TO   HAK-F004.
     MOVE    CENTCD          TO   HAK-FA01.
     MOVE    NOUHIN          TO   WK-NOUHIN.
     MOVE    WK-NOUHIN2      TO   HAK-FA05.
     MOVE    TOKCD           TO   HAK-FA02.
     IF      TOKCD  =  23631
             MOVE    2363    TO   HAK-FA02
     END-IF.
     MOVE    ZERO            TO   HAK-FA03.
     MOVE    ZERO            TO   HAK-FA06.
     MOVE    ZERO            TO   HAK-FA04.
     START   KTHAKOF     KEY  >=  HAK-F001 HAK-F002 HAK-F003
                                  HAK-F004 HAK-FA01 HAK-FA05
                                  HAK-FA02 HAK-FA03 HAK-FA06
                                  HAK-FA04
       INVALID      KEY
             MOVE       1         TO   HAK-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   HAK-FLG
     END-START.
*
 KTHAKOF-START-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.1.2   箱数ファイル　　　ＲＥＡＤ                *
*----------------------------------------------------------*
 KTHAKOF-READ-SUB        SECTION.
*
     READ    KTHAKOF    NEXT
         AT END
             MOVE      1     TO   HAK-FLG
             GO              TO   KTHAKOF-READ-END
         NOT AT END
             MOVE      ZERO  TO   HAK-FLG
     END-READ.
*T
*    DISPLAY "BDATE       = " BDATE        UPON CONS.
*    DISPLAY "BTIME       = " BTIME        UPON CONS.
*    DISPLAY "TOKCD       = " TOKCD        UPON CONS.
*    DISPLAY "SOKCD       = " SOKCD        UPON CONS.
*    DISPLAY "CENTCD      = " CENTCD       UPON CONS.
*    DISPLAY "NOUHIN      = " NOUHIN       UPON CONS.
*    DISPLAY "HAK-F001    = " HAK-F001     UPON CONS.
*    DISPLAY "HAK-F002    = " HAK-F002     UPON CONS.
*    DISPLAY "HAK-F003    = " HAK-F003     UPON CONS.
*    DISPLAY "HAK-F004    = " HAK-F004     UPON CONS.
*    DISPLAY "HAK-FA01    = " HAK-FA01     UPON CONS.
*    DISPLAY "HAK-FA05    = " HAK-FA05     UPON CONS.
*    DISPLAY "HAK-FA02    = " HAK-FA02     UPON CONS.
*    DISPLAY "HAK-FA03    = " HAK-FA03     UPON CONS.
*    DISPLAY "HAK-FA06    = " HAK-FA06     UPON CONS.
*    DISPLAY "HAK-FA04    = " HAK-FA04     UPON CONS.
*T
     MOVE    NOUHIN          TO   WK-NOUHIN.
     IF      HAK-F001  NOT =  BDATE
     OR      HAK-F002  NOT =  BTIME
     OR      HAK-F003  NOT =  TOKCD
     OR      HAK-F004  NOT =  SOKCD
     OR      HAK-FA01  NOT =  CENTCD
     OR      HAK-FA05  NOT =  WK-NOUHIN2
             MOVE      1     TO   HAK-FLG
     END-IF.
*
 KTHAKOF-READ-END.
     EXIT.
*==========================================================*
*      2.2       ボディ部入力              MAIN-FLG=2      *
*==========================================================*
 BODY-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG02         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE   "BODY"           TO   DSP-GROUP.
*
     PERFORM         DSP-READ-SUB.
*
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE     SPACE TO   FSY13221
                        PERFORM             HEADDEL-SUB
                        PERFORM             BODYDEL-SUB
         WHEN   "F005"
                        MOVE     "END" TO   END-FLG
         WHEN   "F006"
                        PERFORM             EDIT-SET-BODY
                        MOVE    "1"    TO   MAIN-FLG
         WHEN   "F011"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   KTHAKOF-UPD-SUB
****************************PERFORM   BODYDEL-SUB
                            PERFORM   BEFORE-PAGE-SUB
                        END-IF
         WHEN   "F012"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   KTHAKOF-UPD-SUB
****************************PERFORM   BODYDEL-SUB
                            PERFORM   NEXT-PAGE-SUB
                        END-IF
         WHEN   "E000"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            MOVE   "3"     TO   MAIN-FLG
                        END-IF
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 BODY-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.1     ボディ部の入力チェック                    *
*----------------------------------------------------------*
 BODYCHK-SUB            SECTION.
*
     PERFORM  EDIT-SET-BODY.
*
     MOVE     ZERO      TO   ERR-MSG-CD.
*
*        ＢＯＤＹ部チェック
     IF   BUMON (IXA)    IS NUMERIC
          PERFORM   BODYCHK1-SUB
     END-IF.
*    END-PERFORM.
*
 BODYCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.1     ボディ部の入力チェック                    *
*----------------------------------------------------------*
 BODYCHK1-SUB           SECTION.
*
*    MOVE     TOKCD                    TO   JYO-F01.
*    MOVE     HACCD(IXA)               TO   JYO-F03.
*    PERFORM  JYOKEN1-READ-SEC
*    IF  JYOKEN1-INV-FLG  =  SPACE
*             MOVE   JYO-F03           TO   HACCD(IXA)
*             MOVE   JYO-F08           TO   HACNM(IXA)
*    ELSE
*        IF   ERR-MSG-CD   =    ZERO
*             MOVE  17  TO   ERR-MSG-CD
*             MOVE  "C" TO   EDIT-CURSOR  OF  HACCD(IXA)
*        END-IF
*        MOVE   "R"     TO   EDIT-OPTION  OF  HACCD(IXA)
*        MOVE   SPACE   TO   BUMONM(IXA)
*    END-IF.
*    部門ＣＤ
*    IF     ( BUMON(IXA) >= 001 AND BUMON(IXA) <= 030 )  OR
*           ( BUMON(IXA) =  091 ) OR
*           ( BUMON(IXA) =  095 )
*        EVALUATE     BUMON(IXA)
*        WHEN    001
*        MOVE NC"園芸用品・大型機械"            TO   BUMONM(IXA)
*        WHEN    002
*        MOVE NC"農業・業務資材"                TO   BUMONM(IXA)
*        WHEN    003
*        MOVE NC"用土・肥料・薬品"              TO   BUMONM(IXA)
*        WHEN    004
*        MOVE NC"植物"                          TO   BUMONM(IXA)
*        WHEN    005
*        MOVE NC"エクステリア・屋外資材"        TO   BUMONM(IXA)
*        WHEN    006
*        MOVE NC"作業用品"                      TO   BUMONM(IXA)
*        WHEN    007
*        MOVE NC"金物"                          TO   BUMONM(IXA)
*        WHEN    008
*        MOVE NC"工具"                          TO   BUMONM(IXA)
*        WHEN    009
*        MOVE NC"塗料・補修"                    TO   BUMONM(IXA)
*        WHEN    010
*        MOVE NC"木材・建築資材"                TO   BUMONM(IXA)
*        WHEN    011
*        MOVE NC"カー用品"                      TO   BUMONM(IXA)
*        WHEN    012
*        MOVE NC"スポーツ・玩具"                TO   BUMONM(IXA)
*        WHEN    013
*        MOVE NC"サイクル・レジャー"            TO   BUMONM(IXA)
*        WHEN    014
*        MOVE NC"ペット"                        TO   BUMONM(IXA)
*        WHEN    015
*        MOVE NC"日用消耗品"                    TO   BUMONM(IXA)
*        WHEN    016
*        MOVE NC"文具"                          TO   BUMONM(IXA)
*        WHEN    017
*        MOVE NC"ダイニング・キッチン"          TO   BUMONM(IXA)
*        WHEN    018
*        MOVE NC"バス・トイレタリー"            TO   BUMONM(IXA)
*        WHEN    019
*        MOVE NC"ＨＢＣ"                        TO   BUMONM(IXA)
*        WHEN    020
*        MOVE NC"食品・酒"                      TO   BUMONM(IXA)
*        WHEN    021
*        MOVE NC"インテリア"                    TO   BUMONM(IXA)
*        WHEN    022
*        MOVE NC"寝具"                          TO   BUMONM(IXA)
*        WHEN    023
*        MOVE NC"家具収納"                      TO   BUMONM(IXA)
*        WHEN    024
*        MOVE NC"家庭電器"                      TO   BUMONM(IXA)
*        WHEN    025
*        MOVE NC"冷暖房・住宅設備"              TO   BUMONM(IXA)
*        WHEN    026
*        MOVE NC"電材・照明"                    TO   BUMONM(IXA)
*        WHEN    027
*        MOVE NC"ＡＶ情報・カウンター商品"      TO   BUMONM(IXA)
*        WHEN    028
*        MOVE NC"テナント植物"                  TO   BUMONM(IXA)
*        WHEN    029
*        MOVE NC"テナントペット"                TO   BUMONM(IXA)
*        WHEN    030
*        MOVE NC"灯油"                          TO   BUMONM(IXA)
*        WHEN    091
*        MOVE NC"工事費"                        TO   BUMONM(IXA)
*        WHEN    095
*        MOVE NC"催事"                          TO   BUMONM(IXA)
*        WHEN    OTHER
*        MOVE NC"＊＊＊＊＊＊"                  TO   BUMONM(IXA)
*        END-EVALUATE
*
*        COMPUTE   NIGOK(IXA) = KOMONO(IXA) + IKEI(IXA) +
*                               KESU  (IXA) + HOKA(IXA)
*    ELSE
*        IF   ERR-MSG-CD   =    ZERO
*             MOVE  16  TO   ERR-MSG-CD
*             MOVE  "C" TO   EDIT-CURSOR  OF  BUMON(IXA)
*        END-IF
*        MOVE   "R"     TO   EDIT-OPTION  OF  BUMON(IXA)
*        MOVE   SPACE   TO   BUMONM(IXA)
*    END-IF.
*    合計
*
*
 BODYCHK1-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.2     前頁処理                                  *
*----------------------------------------------------------*
 BEFORE-PAGE-SUB     SECTION.
*
     PERFORM  KTHAKOF-START3-SUB.
     IF       HAK-FLG   =    ZERO
              PERFORM   KTHAKOF-READ-SUB
              IF   HAK-FLG   NOT =     ZERO
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE     10    TO   ERR-MSG-CD
                   END-IF
                   GO   TO   BEFORE-PAGE-EXIT
              END-IF
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE     10    TO   ERR-MSG-CD
              END-IF
              GO   TO   BEFORE-PAGE-EXIT
     END-IF.
     MOVE     ZERO      TO   SAV-BKEY.
     PERFORM  BODYDEL-SUB.
     PERFORM  VARYING   IXA  FROM  14 BY  -1
              UNTIL   ( IXA       =    0 )  OR
                      ( HAK-FLG   =    1 )
*        ルート
              MOVE      HAK-FA03         TO   ROUTE(IXA)
*        部門ＣＤ
         MOVE      HAK-FA06        TO   BUMON (IXA)
*        部門名
         MOVE     28                        TO   JYO-F01
         MOVE     HAK-FA06                  TO   JYO-F02
         PERFORM  JYOKEN1-READ-SEC
         IF       JYOKEN1-INV-FLG  =  SPACE
                  MOVE   HAK-FA06           TO   BUMON(IXA)
                  MOVE   JYO-F03            TO   BUMONM(IXA)
         ELSE
                  MOVE   HAK-FA06           TO   BUMON(IXA)
                  MOVE   NC"＊＊＊＊＊＊"   TO   BUMONM(IXA)
         END-IF
*
*        発注日
         MOVE      "3"            TO       LINK-IN-KBN
         MOVE      HAK-FA04       TO       LINK-IN-YMD6
         MOVE      ZERO           TO       LINK-IN-YMD8
         MOVE      ZERO           TO       LINK-OUT-RET
         MOVE      ZERO           TO       LINK-OUT-YMD
         CALL      "SKYDTCKB"     USING    LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD
         MOVE      LINK-OUT-YMD   TO       HDATE(IXA)
*
*        箱数
         MOVE      HAK-FB01       TO       HAKOSU(IXA)
*
         IF        IXA  =    1
              MOVE HAK-FA03       TO       SAV-BKEY-ROUTE
              MOVE HAK-FA06       TO       SAV-BKEY-BUMON
              MOVE HAK-FA04       TO       SAV-BKEY-HDATE
         END-IF
         IF        IXA  =    14
              MOVE HAK-FA03       TO       SAV-NKEY-ROUTE
              MOVE HAK-FA06       TO       SAV-NKEY-BUMON
              MOVE HAK-FA04       TO       SAV-NKEY-HDATE
         END-IF
*
         PERFORM   KTHAKOF-READ-SUB
     END-PERFORM.
*
 BEFORE-PAGE-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     次頁処理                                  *
*----------------------------------------------------------*
 NEXT-PAGE-SUB     SECTION.
*
     PERFORM  KTHAKOF-START2-SUB.
     IF       HAK-FLG   =    ZERO
              PERFORM   KTHAKOF-READ-SUB
              IF   HAK-FLG   NOT =     ZERO
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE     11    TO   ERR-MSG-CD
                   END-IF
                   GO   TO   NEXT-PAGE-EXIT
              END-IF
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE     11    TO   ERR-MSG-CD
              END-IF
              GO   TO   NEXT-PAGE-EXIT
     END-IF.
     MOVE     ZERO      TO   SAV-NKEY.
     PERFORM  BODYDEL-SUB.
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL   ( IXA       >    14 )  OR
                      ( HAK-FLG   =    1  )
*        ルート
              MOVE      HAK-FA03         TO   ROUTE(IXA)
*        部門ＣＤ
         MOVE      HAK-FA06        TO   BUMON (IXA)
*********DISPLAY  "HAK-FA06 = " HAK-FA06  UPON CONS
*        部門名
         MOVE     28                        TO   JYO-F01
         MOVE     HAK-FA06                  TO   JYO-F02
         PERFORM  JYOKEN1-READ-SEC
         IF       JYOKEN1-INV-FLG  =  SPACE
                  MOVE   HAK-FA06           TO   BUMON(IXA)
                  MOVE   JYO-F03            TO   BUMONM(IXA)
         ELSE
                  MOVE   HAK-FA06           TO   BUMON(IXA)
                  MOVE   NC"＊＊＊＊＊＊"   TO   BUMONM(IXA)
         END-IF
*        発注日
         MOVE      "3"            TO       LINK-IN-KBN
         MOVE      HAK-FA04       TO       LINK-IN-YMD6
         MOVE      ZERO           TO       LINK-IN-YMD8
         MOVE      ZERO           TO       LINK-OUT-RET
         MOVE      ZERO           TO       LINK-OUT-YMD
         CALL      "SKYDTCKB"     USING    LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD
         MOVE      LINK-OUT-YMD   TO       HDATE(IXA)
*
*        箱数
         MOVE      HAK-FB01       TO       HAKOSU(IXA)
*
         IF        IXA  =    1
              MOVE HAK-FA03       TO       SAV-BKEY-ROUTE
              MOVE HAK-FA06       TO       SAV-BKEY-BUMON
              MOVE HAK-FA04       TO       SAV-BKEY-HDATE
         END-IF
*********IF        IXA  =    14
              MOVE HAK-FA03       TO       SAV-NKEY-ROUTE
              MOVE HAK-FA06       TO       SAV-NKEY-BUMON
              MOVE HAK-FA04       TO       SAV-NKEY-HDATE
*********END-IF
*
         PERFORM   KTHAKOF-READ-SUB

     END-PERFORM.
*
 NEXT-PAGE-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     箱数ファイル　　　ＳＴＡＲＴ２            *
*----------------------------------------------------------*
 KTHAKOF-START2-SUB       SECTION.
*
     MOVE    ZERO            TO   HAK-FLG.
     MOVE    SPACE           TO   HAK-REC.
     INITIALIZE                   HAK-REC.
     MOVE    BDATE           TO   HAK-F001.
     MOVE    BTIME           TO   HAK-F002.
     MOVE    TOKCD           TO   HAK-F003.
     MOVE    SOKCD           TO   HAK-F004.
     MOVE    CENTCD          TO   HAK-FA01.
     MOVE    NOUHIN          TO   WK-NOUHIN.
     MOVE    WK-NOUHIN2      TO   HAK-FA05.
     MOVE    TOKCD           TO   HAK-FA02.
     IF      TOKCD   =  23631
             MOVE  2363      TO   HAK-FA02
     END-IF.
     MOVE    SAV-NKEY-ROUTE  TO   HAK-FA03.
     MOVE    SAV-NKEY-BUMON  TO   HAK-FA06.
     MOVE    SAV-NKEY-HDATE  TO   HAK-FA04.
     START   KTHAKOF     KEY  >   HAK-F001 HAK-F002 HAK-F003
                                  HAK-F004 HAK-FA01 HAK-FA05
                                  HAK-FA02 HAK-FA03 HAK-FA06
                                  HAK-FA04
       INVALID      KEY
             MOVE       1         TO   HAK-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   HAK-FLG
     END-START.
 KTHAKOF-START2-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     箱数ファイル　　　ＳＴＡＲＴ３            *
*----------------------------------------------------------*
 KTHAKOF-START3-SUB       SECTION.
*
     MOVE    ZERO            TO   HAK-FLG.
     MOVE    SPACE           TO   HAK-REC.
     INITIALIZE                   HAK-REC.
     MOVE    BDATE           TO   HAK-F001.
     MOVE    BTIME           TO   HAK-F002.
     MOVE    TOKCD           TO   HAK-F003.
     MOVE    SOKCD           TO   HAK-F004.
     MOVE    CENTCD          TO   HAK-FA01.
     MOVE    NOUHIN          TO   WK-NOUHIN.
     MOVE    WK-NOUHIN2      TO   HAK-FA05.
     MOVE    TOKCD           TO   HAK-FA02.
     IF      TOKCD   =  23631
             MOVE  2363      TO   HAK-FA02
     END-IF.
     MOVE    SAV-BKEY-ROUTE  TO   HAK-FA03.
     MOVE    SAV-BKEY-BUMON  TO   HAK-FA06.
     MOVE    SAV-BKEY-HDATE  TO   HAK-FA04.
     START   KTHAKOF     KEY  <   HAK-F001 HAK-F002 HAK-F003
                                  HAK-F004 HAK-FA01 HAK-FA05
                                  HAK-FA02 HAK-FA03 HAK-FA06
                                  HAK-FA04
                        WITH      REVERSED  ORDER
       INVALID      KEY
             MOVE       1         TO   HAK-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   HAK-FLG
     END-START.
 KTHAKOF-START3-END.
     EXIT.
*----------------------------------------------------------*
*      2.5       確認入力                                  *
*----------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG03         TO   PFMSG.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "KAKU"          TO    DSP-GROUP.
     PERFORM       DSP-READ-SUB.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        PERFORM   HEADDEL-SUB
                        PERFORM   BODYDEL-SUB
         WHEN   "F006"
                        MOVE     "2"   TO   MAIN-FLG
         WHEN   "E000"
                        PERFORM   KAKUCHK-SUB
         WHEN    OTHER
                        MOVE      01   TO   ERR-MSG-CD
     END-EVALUATE.
 KAKUNIN-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1     確認の入力チェック                        *
*----------------------------------------------------------*
 KAKUCHK-SUB             SECTION.
     MOVE       "M"     TO   EDIT-OPTION  OF  KAKU.
     MOVE       SPACE   TO   EDIT-CURSOR  OF  KAKU.
     MOVE       ZERO    TO   ERR-MSG-CD.
*
     PERFORM       KTHAKOF-UPD-SUB.
*****PERFORM       HEADDEL-SUB.
     PERFORM       BODYDEL-SUB.
     MOVE    "1"   TO  MAIN-FLG.
*
 KAKUCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.2  箱数ファイルデータ更新                   *
*----------------------------------------------------------*
 KTHAKOF-UPD-SUB          SECTION.
*
     PERFORM  VARYING   IXA       FROM    1  BY  1
              UNTIL     IXA  >    14
*        未入力かどうか発注日NUMERICで判断
         IF   HDATE(IXA)  IS    NUMERIC  AND
              HDATE(IXA)  NOT = ZERO
              PERFORM   HAK-READ-SUB
              IF   INVALID-FLG    =    1
                   DISPLAY NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊"
                                                UPON CONS
                   DISPLAY NC"箱数レコードなし　更新不可！？"
                                                UPON CONS
                   DISPLAY NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊"
                                                UPON CONS
                   MOVE 4010           TO   PROGRAM-STATUS
                   STOP RUN
              ELSE
                   MOVE HAKOSU(IXA)    TO   HAK-FB01
                   MOVE SYS-DATE2      TO   HAK-FC05
                   MOVE WK-TIME(1:6)   TO   HAK-FC06
                   MOVE PARA-BUMON     TO   HAK-FC07
                   MOVE PARA-TANTOU    TO   HAK-FC08
                   REWRITE   HAK-REC
              END-IF
         END-IF
     END-PERFORM.
*
 KTHAKOF-UPD-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
*
     CLOSE    DSPF KTHAKOF  ZSOKMS HTOKMS HTENMS JYOKEN1.
*
 END-END.
     EXIT.
************************************************************
*      9.0        共通処理                                 *
************************************************************
*==========================================================*
*      9.1       エラー メッセージセット                   *
*==========================================================*
 MSG-SEC                SECTION.
     IF  ERR-MSG-CD     =    ZERO
         MOVE      SPACE     TO   ERRMSG
     ELSE
         MOVE      ERR-MSG(ERR-MSG-CD)      TO   ERRMSG
         MOVE      ZERO      TO   ERR-MSG-CD
     END-IF.
 MSG-END.
     EXIT.
*==========================================================*
*      9.2       項目制御部初期化                          *
*==========================================================*
*----------------------------------------------------------*
*      9.2.1     項目制御部初期化（ヘッド部）              *
*----------------------------------------------------------*
 EDIT-SET-HEAD          SECTION.
     MOVE   "M"     TO   EDIT-OPTION  OF  BDATE.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  BDATE.
     MOVE   "M"     TO   EDIT-OPTION  OF  BTIME.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  BTIME.
     MOVE   "M"     TO   EDIT-OPTION  OF  TOKCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TOKCD.
     MOVE   "M"     TO   EDIT-OPTION  OF  SOKCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SOKCD.
     MOVE   "M"     TO   EDIT-OPTION  OF  CENTCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  CENTCD.
     MOVE   "M"     TO   EDIT-OPTION  OF  NOUHIN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  NOUHIN.
 EDIT-SET-HEAD-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.3     項目制御部初期化（ボディ部）              *
*----------------------------------------------------------*
 EDIT-SET-BODY          SECTION.
     PERFORM  VARYING IXB     FROM  1  BY  1
              UNTIL   IXB     >     14
         PERFORM     EDIT-SET-GYO
     END-PERFORM.
 EDIT-SET-BODY-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.3.1   項目制御部初期化（ボディ部１行）          *
*----------------------------------------------------------*
 EDIT-SET-GYO           SECTION.
*
     MOVE   "M"     TO   EDIT-OPTION  OF  HAKOSU(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HAKOSU(IXB).
*
 EDIT-SET-GYO-END.
     EXIT.
*==========================================================*
*      9.3       画面表示処理                              *
*==========================================================*
 DSP-WRITE-SUB          SECTION.
*    倉庫ＣＤチェック
     IF       PARA-DSOKCD  = "01"  OR  "88"
              MOVE    " "      TO   EDIT-STATUS OF SOKCD
     ELSE
       DISPLAY "PARA-SOKCD = " PARA-SOKCD UPON CONS
              MOVE PARA-SOKCD  TO   SOK-F01 SOKCD
              PERFORM ZSOKMS-READ-SEC
              IF  ZSOKMS-INV-FLG = SPACE
                  MOVE SOK-F02 TO   SOKNM
              ELSE
                  MOVE ALL NC"＊" TO SOKNM
              END-IF
              MOVE "X"     TO   EDIT-STATUS OF SOKCD
     END-IF.
     MOVE     HEN-DATE        TO   SDATE.
     MOVE     HEN-TIME        TO   STIME.
     MOVE    "FSY13221"       TO   DSP-FORMAT.
     MOVE    "SCREEN"         TO   DSP-GROUP.
     MOVE     SPACE           TO   DSP-PROC.
     WRITE    FSY13221.
 DSP-WRITE-END.
     EXIT.
*==========================================================*
*      9.4       画面データの入力処理                      *
*==========================================================*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"    TO   DSP-PROC.
     READ   DSPF.
 DSP-READ-END.
     EXIT.
*==========================================================*
*      9.5       ＨＥＡＤ部消去                            *
*==========================================================*
 HEADDEL-SUB            SECTION.
     MOVE   SPACE       TO   MAS003.
     PERFORM            EDIT-SET-HEAD.
 HEADDEL-END.
     EXIT.
*==========================================================*
*      9.6       ＢＯＤＹ部消去                            *
*==========================================================*
 BODYDEL-SUB            SECTION.
     MOVE  SPACE        TO   MAS001.
     PERFORM            EDIT-SET-BODY.
 BODYDEL-END.
     EXIT.
*==========================================================*
*      9.7       ファイル処理                              *
*==========================================================*
*----------------------------------------------------------*
*      9.7.3     箱数ファイル　　　検索                    *
*----------------------------------------------------------*
 HAK-READ-SUB           SECTION.
     MOVE    ZERO            TO   INVALID-FLG.
     MOVE    SPACE           TO   HAK-REC.
     INITIALIZE                   HAK-REC.
     MOVE    BDATE           TO   HAK-F001.
     MOVE    BTIME           TO   HAK-F002.
     MOVE    TOKCD           TO   HAK-F003.
     MOVE    SOKCD           TO   HAK-F004.
     MOVE    CENTCD          TO   HAK-FA01.
     MOVE    NOUHIN          TO   WK-NOUHIN.
     MOVE    WK-NOUHIN2      TO   HAK-FA05.
     MOVE    TOKCD           TO   HAK-FA02.
     IF      TOKCD   =  23631
             MOVE  2363      TO   HAK-FA02
     END-IF.
     MOVE    ROUTE (IXA)     TO   HAK-FA03.
     MOVE    BUMON (IXA)     TO   HAK-FA06.
     MOVE    HDATE (IXA)     TO   WK-HACYU.
     MOVE    WK-HACYU2       TO   HAK-FA04.
     READ    KTHAKOF
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 HAK-READ-END.
     EXIT.
*----------------------------------------------------------*
*                倉庫マスタ                                *
*----------------------------------------------------------*
 ZSOKMS-READ-SEC        SECTION.
*
     READ    ZSOKMS
       INVALID      KEY
          MOVE     "INV"     TO   ZSOKMS-INV-FLG
       NOT INVALID  KEY
          MOVE      SPACE    TO   ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*                取引先マスタ                              *
*----------------------------------------------------------*
 HTOKMS-READ-SEC       SECTION.
*
     READ    HTOKMS
       INVALID      KEY
          MOVE     "INV"     TO   HTOKMS-INV-FLG
       NOT INVALID  KEY
          MOVE      SPACE    TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*                店舗マスタ                            *
*----------------------------------------------------------*
 HTENMS-READ-SEC       SECTION.
*
     READ    HTENMS
       INVALID      KEY
          MOVE     "INV"     TO   HTENMS-INV-FLG
       NOT INVALID  KEY
          MOVE      SPACE    TO   HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*　　条件ファイル索引
****************************************************************
 JYOKEN1-READ-SEC         SECTION.
*
     READ     JYOKEN1
         INVALID
           MOVE  "INV"     TO        JYOKEN1-INV-FLG
         NOT INVALID
           MOVE  SPACE     TO        JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
