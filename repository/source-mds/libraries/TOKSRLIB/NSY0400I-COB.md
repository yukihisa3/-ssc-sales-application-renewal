# NSY0400I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NSY0400I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭ仕入先統合　　　　　　　　　*
*    モジュール名　　　　：　センター納入情報入力　　　　　　　*
*    作成日／更新日　　　：　2021/02/18                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　センター納入情報の入力を行います。*
*                                                              *
**履歴**********************************************************
*    2021/02/18  高橋　　新規作成（ＳＳＹ９２０４Ｉコピー）　　*
*    0000/00/00　　　　　　　　　　　　　　　　　　　　　　　　*
*    0000/00/00　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NSY0400I.
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
*---<<  ダイキセンター納品データ  >>---*
     SELECT   DNCENTF   ASSIGN    TO        DA-01-VI-DNCENTL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       DKC-F01   DKC-F02
                                            DKC-F03   DKC-F04
                                            DKC-F05   DKC-F06
                                            DKC-F19   DKC-F07
                        FILE      STATUS    IS   DKC-STATUS.
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
*#2019/03/11 NAV ST
*発注種別変換マスタ
     SELECT   DCMHSBL1  ASSIGN    TO        DA-01-VI-DCMHSBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HSB-F01  HSB-F02
                        FILE  STATUS   IS   HSB-STATUS.
*#2019/03/11 NAV ED
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
 01  DSP-REC            PIC  X(2000).
     COPY     FSY04001  OF        XMDLIB.
*---<<  ダイキセンター納品データ  >>---*
 FD  DNCENTF.
     COPY     DNCENTF   OF        XFDLIB
              JOINING   DKC       PREFIX.
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
*#2019/03/18 NAV ST
*---<<  発注種別変換マスタ  >>---*
 FD  DCMHSBL1.
     COPY     DCMHSBL1  OF        XFDLIB
              JOINING   HSB       PREFIX.
*#2019/03/18 NAV ED
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
     02  DKC-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
     02  SOK-STATUS          PIC  X(02).
     02  TEN-STATUS          PIC  X(02).
*#2019/03/18 NAV ST
     02  HSB-STATUS          PIC  X(02).
*#2019/03/18 NAV ED
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  READ-FLG            PIC  X(01)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
     02  DKC-FLG             PIC  9(01)  VALUE ZERO.
     02  SOKKENF-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  ZSOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  HTOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  HTENMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  SOKKPGF-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  WK-CHK-BUMON        PIC  9(03)  VALUE ZERO.
*#2019/03/18 NAV ST
     02  DCMHSBL1-INV-FLG    PIC  X(03)  VALUE SPACE.
*#2019/03/18 NAV ED
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
       05  SAV-BKEY-KBN        PIC  X(02)   VALUE  ZERO.
       05  SAV-BKEY-BUMON      PIC  9(03)   VALUE  ZERO.
     03  SAV-NKEY.
       05  SAV-NKEY-KBN        PIC  X(02)   VALUE  ZERO.
       05  SAV-NKEY-BUMON      PIC  9(03)   VALUE  ZERO.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NSY0400I".
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
             NC"バッチＮＯを入力して下さい。".
     02  MSG-ERR7            PIC  N(20)  VALUE
             NC"店舗マスタに未登録です。".
     02  MSG-ERR8            PIC  N(20)  VALUE
             NC"バッチＮＯ（日付）論理エラー".
     02  MSG-ERR9            PIC  N(20)  VALUE
             NC"バッチＮＯ（時間）論理エラー".
     02  MSG-ERR10           PIC  N(20)  VALUE
             NC"前頁がありません。".
     02  MSG-ERR11           PIC  N(20)  VALUE
             NC"次頁がありません。".
     02  MSG-ERR12           PIC  N(20)  VALUE
             NC"センターＣＤを入力して下さい。".
     02  MSG-ERR13           PIC  N(20)  VALUE
             NC"納品日を入力して下さい。".
     02  MSG-ERR14           PIC  N(20)  VALUE
             NC"納品日（日付）論理エラー".
     02  MSG-ERR15           PIC  N(20)  VALUE
             NC"該当データが存在しません。".
     02  MSG-ERR16           PIC  N(20)  VALUE
             NC"部門ＣＤに誤りがあります。".
     02  MSG-ERR17           PIC  N(20)  VALUE
             NC"定番特売区分が違います。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(20)
                             OCCURS 17  TIMES.
*画面キー項目セット
 01  WK-KEY-SET.
     02  WK-BDATE            PIC  9(08)  VALUE  ZERO.
     02  WK-BTIME            PIC  9(04)  VALUE  ZERO.
     02  WK-TOKCD            PIC  9(08)  VALUE  ZERO.
     02  WK-SOKCD            PIC  X(02)  VALUE  SPACE.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
 LINKAGE                SECTION.
 01  PARA-SOKCD            PIC X(02).
 01  PARA-DSOKCD           PIC X(02).
*----------------------------------------------------------*
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
*----------------------------------------------------------*
 PROCEDURE              DIVISION  USING  PARA-SOKCD PARA-DSOKCD.
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
 FILEERR-DKCE           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DNCENTF.
     MOVE     "DNCENTF"        TO   ERR-FL-ID.
     MOVE      DKC-STATUS      TO   ERR-STCD.
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
                        PROCEDURE   DCMHSBL1.
     MOVE     "DCMHSBL1"       TO   ERR-FL-ID.
     MOVE      HSB-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 END     DECLARATIVES.
************************************************************
 NSY0400I-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 NSY0400I-END.
     EXIT.
************************************************************
*      ■０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O   DSPF    DNCENTF.
     OPEN     INPUT HTOKMS ZSOKMS HTENMS.
*#2019/03/18 NAV ST
     OPEN     INPUT DCMHSBL1.
*#2019/03/18 NAV ED
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
     MOVE     SPACE          TO   FSY04001.
     PERFORM       DSP-WRITE-SUB.
     PERFORM       EDIT-SET-HEAD.
     PERFORM       EDIT-SET-BODY.
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
                        MOVE     SPACE TO   FSY04001
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
                             PERFORM   DKCNTF-DSP-SUB
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
*バッチNO（日付）チェック
***  バッチNO（日付）未入力チェック
     IF       BDATE  NOT NUMERIC
         OR   BDATE  =  ZERO
              MOVE   06      TO   ERR-MSG-CD
              MOVE  "R"      TO   EDIT-OPTION  OF  BDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  BDATE
     ELSE
***           バッチNO（日付）論理チェック
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
*バッチNO（時間）チェック
***  バッチNO（時間）未入力チェック
     IF       BTIME     NOT NUMERIC
         OR   BTIME     =   ZERO
                   IF     ERR-MSG-CD   =  ZERO
                          MOVE   06      TO   ERR-MSG-CD
                   END-IF
                   MOVE  "R"      TO   EDIT-OPTION  OF  BTIME
                   MOVE  "C"      TO   EDIT-CURSOR  OF  BTIME
     ELSE
***           バッチNO（時間）論理チェック
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
*バッチNO（取引先）チェック
***  バッチNO（取引先）未入力チェック
     IF       TOKCD    NOT NUMERIC
         OR   TOKCD    =  ZERO
              IF     ERR-MSG-CD   =  ZERO
                     MOVE   05      TO   ERR-MSG-CD
              END-IF
              MOVE  "R"      TO   EDIT-OPTION  OF  TOKCD
              MOVE  "C"      TO   EDIT-CURSOR  OF  TOKCD
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
***  センターＣＤ未入力チェック
     IF       CENTCD    NOT NUMERIC
         OR   CENTCD    =    ZERO
              IF   ERR-MSG-CD     =    ZERO
                   MOVE   12      TO   ERR-MSG-CD
                   MOVE  "C"      TO   EDIT-CURSOR  OF  CENTCD
              END-IF
              MOVE  "R"      TO   EDIT-OPTION  OF  CENTCD
     ELSE
***           センターＣＤチェック
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
 DKCNTF-DSP-SUB       SECTION.
*
     PERFORM  EDIT-SET-BODY.
     MOVE     ZERO      TO   ERR-MSG-CD.
*
     MOVE     ZERO      TO   DKC-FLG.
     PERFORM  DKCNTF-START-SUB.
     IF       DKC-FLG   =    ZERO
              PERFORM   DKCNTF-READ-SUB
              IF  DKC-FLG = 1
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
                  GO      TO     DKCNTF-DSP-EXIT
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
              GO      TO     DKCNTF-DSP-EXIT
     END-IF.
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL   ( IXA       >    14 )  OR
                      ( DKC-FLG   =    1  )
*        定番特売区分（名称）
         EVALUATE  DKC-F16
              WHEN    "1"
                   MOVE "1"              TO   HACCD(IXA)
                   MOVE NC"定番：送込含" TO   HACNM(IXA)
              WHEN    "2"
                   MOVE "2"              TO   HACCD(IXA)
                   MOVE NC"特売　　　　" TO   HACNM(IXA)
              WHEN    "3"
                   MOVE "3"              TO   HACCD(IXA)
                   MOVE NC"本部発注　　" TO   HACNM(IXA)
              WHEN    "4"
                   MOVE "4"              TO   HACCD(IXA)
                   MOVE NC"新店発注　　" TO   HACNM(IXA)
              WHEN    "5"
                   MOVE "5"              TO   HACCD(IXA)
                   MOVE NC"増床発注　　" TO   HACNM(IXA)
              WHEN    "6"
                   MOVE "6"              TO   HACCD(IXA)
                   MOVE NC"特注（客注）" TO   HACNM(IXA)
              WHEN    "7"
                   MOVE "7"              TO   HACCD(IXA)
                   MOVE NC"改廃　　　　" TO   HACNM(IXA)
              WHEN    "8"
                   MOVE "8"              TO   HACCD(IXA)
                   MOVE NC"商管補充　　" TO   HACNM(IXA)
              WHEN    "9"
                   MOVE "9"              TO   HACCD(IXA)
                   MOVE NC"ＢＹ改廃　　" TO   HACNM(IXA)
              WHEN    "A"
                   MOVE "A"              TO   HACCD(IXA)
                   MOVE NC"本部在庫補充" TO   HACNM(IXA)
              WHEN    "B"
                   MOVE "B"              TO   HACCD(IXA)
                   MOVE NC"備品用度品　" TO   HACNM(IXA)
              WHEN    "C"
                   MOVE "C"              TO   HACCD(IXA)
                   MOVE NC"プロモーショ" TO   HACNM(IXA)
              WHEN    OTHER
                   MOVE "*"              TO   HACCD(IXA)
                   MOVE NC"＊＊＊＊＊＊" TO   HACNM(IXA)
         END-EVALUATE
*#2019/03/18 NAV ST 発注種別変換マスタより取得
         MOVE     DKC-F03                  TO   HSB-F01
         MOVE     DKC-F19                  TO   HSB-F02
** DISPLAY "DKC-F03 = " DKC-F03  UPON CONS
** DISPLAY "DKC-F19 = " DKC-F19  UPON CONS
         PERFORM  DCMHSBL1-READ-SEC
         IF  DCMHSBL1-INV-FLG  =  SPACE
                  MOVE   DKC-F19           TO   HACCD(IXA)
                  MOVE   HSB-F08           TO   HACNM(IXA)
         ELSE
                   MOVE  DKC-F19           TO   HACCD(IXA)
                   MOVE  NC"＊＊＊＊＊＊"  TO   HACNM(IXA)
         END-IF
**  DISPLAY "DCMHSBL1-INV-FLG = " DCMHSBL1-INV-FLG UPON CONS
**  DISPLAY "HACCD(IXA)       = " HACCD(IXA)       UPON CONS
*#2019/03/18 NAV ST 発注種別変換マスタより取得
*        部門ＣＤ
         MOVE      DKC-F07        TO   BUMON (IXA)
*        部門名
         EVALUATE     DKC-F07
         WHEN    001
         MOVE NC"園芸用品・大型機械"            TO   BUMONM(IXA)
         WHEN    002
         MOVE NC"農業・業務資材"                TO   BUMONM(IXA)
         WHEN    003
         MOVE NC"用土・肥料・薬品"              TO   BUMONM(IXA)
         WHEN    004
         MOVE NC"植物"                          TO   BUMONM(IXA)
         WHEN    005
         MOVE NC"エクステリア・屋外資材"        TO   BUMONM(IXA)
         WHEN    006
         MOVE NC"作業用品"                      TO   BUMONM(IXA)
         WHEN    007
         MOVE NC"金物"                          TO   BUMONM(IXA)
         WHEN    008
         MOVE NC"工具"                          TO   BUMONM(IXA)
         WHEN    009
         MOVE NC"塗料・補修"                    TO   BUMONM(IXA)
         WHEN    010
         MOVE NC"木材・建築資材"                TO   BUMONM(IXA)
         WHEN    011
         MOVE NC"カー用品"                      TO   BUMONM(IXA)
         WHEN    012
         MOVE NC"スポーツ・玩具"                TO   BUMONM(IXA)
         WHEN    013
         MOVE NC"サイクル・レジャー"            TO   BUMONM(IXA)
         WHEN    014
         MOVE NC"ペット"                        TO   BUMONM(IXA)
         WHEN    015
         MOVE NC"日用消耗品"                    TO   BUMONM(IXA)
         WHEN    016
         MOVE NC"文具"                          TO   BUMONM(IXA)
         WHEN    017
         MOVE NC"ダイニング・キッチン"          TO   BUMONM(IXA)
         WHEN    018
         MOVE NC"バス・トイレタリー"            TO   BUMONM(IXA)
         WHEN    019
         MOVE NC"ＨＢＣ"                        TO   BUMONM(IXA)
         WHEN    020
         MOVE NC"食品・酒"                      TO   BUMONM(IXA)
         WHEN    021
         MOVE NC"インテリア"                    TO   BUMONM(IXA)
         WHEN    022
         MOVE NC"寝具"                          TO   BUMONM(IXA)
         WHEN    023
         MOVE NC"家具収納"                      TO   BUMONM(IXA)
         WHEN    024
         MOVE NC"家庭電器"                      TO   BUMONM(IXA)
         WHEN    025
         MOVE NC"冷暖房・住宅設備"              TO   BUMONM(IXA)
         WHEN    026
         MOVE NC"電材・照明"                    TO   BUMONM(IXA)
         WHEN    027
         MOVE NC"ＡＶ情報・カウンター商品"      TO   BUMONM(IXA)
         WHEN    028
         MOVE NC"テナント植物"                  TO   BUMONM(IXA)
         WHEN    029
         MOVE NC"テナントペット"                TO   BUMONM(IXA)
         WHEN    030
         MOVE NC"灯油"                          TO   BUMONM(IXA)
         WHEN    091
         MOVE NC"工事費"                        TO   BUMONM(IXA)
         WHEN    095
         MOVE NC"催事"                          TO   BUMONM(IXA)
         WHEN    OTHER
         MOVE NC"＊＊＊＊＊＊"                  TO   BUMONM(IXA)
         END-EVALUATE
*08/02/20 END
*
*        小物
         MOVE      DKC-F08        TO   KOMONO(IXA)
*        異形
         MOVE      DKC-F09        TO   IKEI  (IXA)
*        ケース
         MOVE      DKC-F10        TO   KESU  (IXA)
*        他
         MOVE      DKC-F11        TO   HOKA  (IXA)
*        合計
         COMPUTE   NIGOK(IXA)  =  DKC-F08  +  DKC-F09  +
                                  DKC-F10  +  DKC-F11
*
         IF        IXA  =    1
              MOVE DKC-F07        TO   SAV-BKEY-BUMON
              MOVE DKC-F19        TO   SAV-BKEY-KBN
         END-IF
         IF        IXA  =    14
              MOVE DKC-F07        TO   SAV-NKEY-BUMON
              MOVE DKC-F19        TO   SAV-NKEY-KBN
         END-IF
*
         PERFORM   DKCNTF-READ-SUB
     END-PERFORM.
*
 DKCNTF-DSP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.1.1.1   センター納品データＳＴＡＲＴ              *
*----------------------------------------------------------*
 DKCNTF-START-SUB        SECTION.
*
     MOVE    ZERO            TO   DKC-FLG.
     MOVE    SPACE           TO   DKC-REC.
     INITIALIZE                   DKC-REC.
     MOVE    BDATE           TO   DKC-F01.
     MOVE    BTIME           TO   DKC-F02.
     MOVE    TOKCD           TO   DKC-F03.
     MOVE    SOKCD           TO   DKC-F04.
     MOVE    CENTCD          TO   DKC-F06.
     MOVE    NOUHIN          TO   DKC-F05.
     START   DNCENTF     KEY  >=  DKC-F01 DKC-F02 DKC-F03
                                  DKC-F04 DKC-F05 DKC-F06
                                  DKC-F19 DKC-F07
       INVALID      KEY
             MOVE       1         TO   DKC-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   DKC-FLG
     END-START.
 DKCNTF-START-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.1.2   センター納品データＲＥＡＤ                *
*----------------------------------------------------------*
 DKCNTF-READ-SUB        SECTION.
*
     READ    DNCENTF    NEXT
         AT END
             MOVE      1     TO   DKC-FLG
             GO              TO   DKCNTF-READ-END
         NOT AT END
             MOVE      ZERO  TO   DKC-FLG
     END-READ.
*
     IF      DKC-F01  NOT =  BDATE
     OR      DKC-F02  NOT =  BTIME
     OR      DKC-F03  NOT =  TOKCD
     OR      DKC-F04  NOT =  SOKCD
     OR      DKC-F06  NOT =  CENTCD
     OR      DKC-F05  NOT =  NOUHIN
             MOVE      1     TO   DKC-FLG
     END-IF.
*
 DKCNTF-READ-END.
     EXIT.
*==========================================================*
*      2.2       ボディ部入力              MAIN-FLG=2      *
*==========================================================*
 BODY-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG02         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE   "BODY"           TO   DSP-GROUP.
     PERFORM  VARYING  IXB   FROM  1  BY  1
              UNTIL    IXB   >     14
         IF   BUMON(IXB)    IS  NOT NUMERIC     OR
            ( BUMON(IXB)    >=  031  AND BUMON(IXB) <= 090 ) OR
            ( BUMON(IXB)    =   092 )  OR
            ( BUMON(IXB)    =   093 )  OR
            ( BUMON(IXB)    =   094 )  OR
            ( BUMON(IXB)    >   095 )
              MOVE    SPACE     TO     BUMONM(IXB)
              MOVE   SPACE TO   EDIT-STATUS  OF  BUMON (IXB)
         ELSE
              MOVE   "X"   TO   EDIT-STATUS  OF  BUMON (IXB)
              MOVE   "X"   TO   EDIT-STATUS  OF  HACCD (IXB)
         END-IF
     END-PERFORM.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE     SPACE TO   FSY04001
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
                            PERFORM   DKCNTF-UPD-SUB
                            PERFORM   BEFORE-PAGE-SUB
                        END-IF
         WHEN   "F012"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   DKCNTF-UPD-SUB
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
     PERFORM  EDIT-SET-BODY.
     MOVE     ZERO      TO   ERR-MSG-CD.
*
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL     IXA    >   14
         IF   BUMON (IXA)    IS NOT NUMERIC
              MOVE   SPACE      TO   BUMONM(IXA)
         END-IF
*        ＢＯＤＹ部チェック
         IF   BUMON (IXA)    IS NUMERIC
              PERFORM   BODYCHK1-SUB
         END-IF
     END-PERFORM.
*
 BODYCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.1     ボディ部の入力チェック                    *
*----------------------------------------------------------*
 BODYCHK1-SUB           SECTION.
*定番特売区分（名称）
*#2019/03/19 NMAV ST
*### IF   HACCD(IXA)  =  "1" OR "2" OR "3" OR "4" OR "5" OR "6"
*###                  OR "7" OR "8" OR "9" OR "A" OR "B" OR "C"
*2013/08/15 NAV ED 発注種別区分変更に伴う追加　７：改廃
*#2019/03/18 NAV ST 発注種別変換マスタより取得
     MOVE     TOKCD                    TO   HSB-F01.
     MOVE     HACCD(IXA)               TO   HSB-F03.
     MOVE     HACCD(IXA)               TO   HSB-F02.
**   DISPLAY "HACCD (IXA) $$ = " HACCD (IXA) UPON CONS.
     PERFORM  DCMHSBL1-READ-SEC
     IF  DCMHSBL1-INV-FLG  =  SPACE
              MOVE   HSB-F02           TO   HACCD(IXA)
              MOVE   HSB-F08           TO   HACNM(IXA)
*####### EVALUATE  HACCD(IXA)
*#######      WHEN    "1"
*#######           MOVE NC"定番：送込含" TO   HACNM(IXA)
*#######      WHEN    "2"
*#######           MOVE NC"特売　　　　" TO   HACNM(IXA)
*#######      WHEN    "3"
*#######           MOVE NC"本部発注　　" TO   HACNM(IXA)
*#######      WHEN    "4"
*#######           MOVE NC"新店発注　　" TO   HACNM(IXA)
*#######      WHEN    "5"
*#######           MOVE NC"増床発注　　" TO   HACNM(IXA)
*#######      WHEN    "6"
*#######           MOVE NC"特注（客注）" TO   HACNM(IXA)
*#######      WHEN    "7"
*#######           MOVE NC"改廃　　　　" TO   HACNM(IXA)
*#######      WHEN    "8"
*#######           MOVE NC"商管補充　　" TO   HACNM(IXA)
*#######      WHEN    "9"
*#######           MOVE NC"ＢＹ改廃　　" TO   HACNM(IXA)
*#######      WHEN    "A"
*#######           MOVE NC"本部在庫補充" TO   HACNM(IXA)
*#######      WHEN    "B"
*#######           MOVE NC"備品用度品　" TO   HACNM(IXA)
*#######      WHEN    "C"
*#######           MOVE NC"プロモーショ" TO   HACNM(IXA)
*#######      WHEN    OTHER
*#######           MOVE NC"＊＊＊＊＊＊" TO   HACNM(IXA)
*####### END-EVALUATE
     ELSE
         IF   ERR-MSG-CD   =    ZERO
              MOVE  17  TO   ERR-MSG-CD
              MOVE  "C" TO   EDIT-CURSOR  OF  HACCD(IXA)
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  HACCD(IXA)
         MOVE   SPACE   TO   BUMONM(IXA)
     END-IF.
*    部門ＣＤ
     IF     ( BUMON(IXA) >= 001 AND BUMON(IXA) <= 030 )  OR
            ( BUMON(IXA) =  091 ) OR
            ( BUMON(IXA) =  095 )
         EVALUATE     BUMON(IXA)
         WHEN    001
         MOVE NC"園芸用品・大型機械"            TO   BUMONM(IXA)
         WHEN    002
         MOVE NC"農業・業務資材"                TO   BUMONM(IXA)
         WHEN    003
         MOVE NC"用土・肥料・薬品"              TO   BUMONM(IXA)
         WHEN    004
         MOVE NC"植物"                          TO   BUMONM(IXA)
         WHEN    005
         MOVE NC"エクステリア・屋外資材"        TO   BUMONM(IXA)
         WHEN    006
         MOVE NC"作業用品"                      TO   BUMONM(IXA)
         WHEN    007
         MOVE NC"金物"                          TO   BUMONM(IXA)
         WHEN    008
         MOVE NC"工具"                          TO   BUMONM(IXA)
         WHEN    009
         MOVE NC"塗料・補修"                    TO   BUMONM(IXA)
         WHEN    010
         MOVE NC"木材・建築資材"                TO   BUMONM(IXA)
         WHEN    011
         MOVE NC"カー用品"                      TO   BUMONM(IXA)
         WHEN    012
         MOVE NC"スポーツ・玩具"                TO   BUMONM(IXA)
         WHEN    013
         MOVE NC"サイクル・レジャー"            TO   BUMONM(IXA)
         WHEN    014
         MOVE NC"ペット"                        TO   BUMONM(IXA)
         WHEN    015
         MOVE NC"日用消耗品"                    TO   BUMONM(IXA)
         WHEN    016
         MOVE NC"文具"                          TO   BUMONM(IXA)
         WHEN    017
         MOVE NC"ダイニング・キッチン"          TO   BUMONM(IXA)
         WHEN    018
         MOVE NC"バス・トイレタリー"            TO   BUMONM(IXA)
         WHEN    019
         MOVE NC"ＨＢＣ"                        TO   BUMONM(IXA)
         WHEN    020
         MOVE NC"食品・酒"                      TO   BUMONM(IXA)
         WHEN    021
         MOVE NC"インテリア"                    TO   BUMONM(IXA)
         WHEN    022
         MOVE NC"寝具"                          TO   BUMONM(IXA)
         WHEN    023
         MOVE NC"家具収納"                      TO   BUMONM(IXA)
         WHEN    024
         MOVE NC"家庭電器"                      TO   BUMONM(IXA)
         WHEN    025
         MOVE NC"冷暖房・住宅設備"              TO   BUMONM(IXA)
         WHEN    026
         MOVE NC"電材・照明"                    TO   BUMONM(IXA)
         WHEN    027
         MOVE NC"ＡＶ情報・カウンター商品"      TO   BUMONM(IXA)
         WHEN    028
         MOVE NC"テナント植物"                  TO   BUMONM(IXA)
         WHEN    029
         MOVE NC"テナントペット"                TO   BUMONM(IXA)
         WHEN    030
         MOVE NC"灯油"                          TO   BUMONM(IXA)
         WHEN    091
         MOVE NC"工事費"                        TO   BUMONM(IXA)
         WHEN    095
         MOVE NC"催事"                          TO   BUMONM(IXA)
         WHEN    OTHER
         MOVE NC"＊＊＊＊＊＊"                  TO   BUMONM(IXA)
         END-EVALUATE
*
         COMPUTE   NIGOK(IXA) = KOMONO(IXA) + IKEI(IXA) +
                                KESU  (IXA) + HOKA(IXA)
     ELSE
         IF   ERR-MSG-CD   =    ZERO
              MOVE  16  TO   ERR-MSG-CD
              MOVE  "C" TO   EDIT-CURSOR  OF  BUMON(IXA)
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  BUMON(IXA)
         MOVE   SPACE   TO   BUMONM(IXA)
     END-IF.
*    合計

*
 BODYCHK1-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.2     前頁処理                                  *
*----------------------------------------------------------*
 BEFORE-PAGE-SUB     SECTION.
*
     PERFORM  DKCNTF-START3-SUB.
     IF       DKC-FLG   =    ZERO
              PERFORM   DKCNTF-READ-SUB
              IF   DKC-FLG   NOT =     ZERO
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
                      ( DKC-FLG   =    1 )
*        定番特売区分（名称）
         EVALUATE  DKC-F16
              WHEN    "1"
                   MOVE "1"              TO   HACCD(IXA)
                   MOVE NC"定番：送込含" TO   HACNM(IXA)
              WHEN    "2"
                   MOVE "2"              TO   HACCD(IXA)
                   MOVE NC"特売　　　　" TO   HACNM(IXA)
              WHEN    "3"
                   MOVE "3"              TO   HACCD(IXA)
                   MOVE NC"本部発注　　" TO   HACNM(IXA)
              WHEN    "4"
                   MOVE "4"              TO   HACCD(IXA)
                   MOVE NC"新店発注　　" TO   HACNM(IXA)
              WHEN    "5"
                   MOVE "5"              TO   HACCD(IXA)
                   MOVE NC"増床発注　　" TO   HACNM(IXA)
              WHEN    "6"
                   MOVE "6"              TO   HACCD(IXA)
                   MOVE NC"特注（客注）" TO   HACNM(IXA)
              WHEN    "7"
                   MOVE "7"              TO   HACCD(IXA)
                   MOVE NC"改廃　　　　" TO   HACNM(IXA)
              WHEN    "8"
                   MOVE "8"              TO   HACCD(IXA)
                   MOVE NC"商管補充　　" TO   HACNM(IXA)
              WHEN    "9"
                   MOVE "9"              TO   HACCD(IXA)
                   MOVE NC"ＢＹ改廃　　" TO   HACNM(IXA)
              WHEN    "A"
                   MOVE "A"              TO   HACCD(IXA)
                   MOVE NC"本部在庫補充" TO   HACNM(IXA)
              WHEN    "B"
                   MOVE "B"              TO   HACCD(IXA)
                   MOVE NC"備品用度品　" TO   HACNM(IXA)
              WHEN    "C"
                   MOVE "C"              TO   HACCD(IXA)
                   MOVE NC"プロモーショ" TO   HACNM(IXA)
              WHEN    OTHER
                   MOVE "*"              TO   HACCD(IXA)
                   MOVE NC"＊＊＊＊＊＊" TO   HACNM(IXA)
         END-EVALUATE
*#2019/03/18 NAV ST 発注種別変換マスタより取得
         MOVE     DKC-F03                  TO   HSB-F01
         MOVE     DKC-F19                  TO   HSB-F03
         PERFORM  DCMHSBL1-READ-SEC
         IF  DCMHSBL1-INV-FLG  =  SPACE
                  MOVE   DKC-F19           TO   HACCD(IXA)
                  MOVE   HSB-F08           TO   HACNM(IXA)
         ELSE
                   MOVE  DKC-F19           TO   HACCD(IXA)
                   MOVE  NC"＊＊＊＊＊＊"  TO   HACNM(IXA)
         END-IF
*#2019/03/18 NAV ED 発注種別変換マスタより取得
*        部門ＣＤ
         MOVE      DKC-F07        TO   BUMON (IXA)
*        部門名
         EVALUATE     DKC-F07
         WHEN    001
         MOVE NC"園芸用品・大型機械"            TO   BUMONM(IXA)
         WHEN    002
         MOVE NC"農業・業務資材"                TO   BUMONM(IXA)
         WHEN    003
         MOVE NC"用土・肥料・薬品"              TO   BUMONM(IXA)
         WHEN    004
         MOVE NC"植物"                          TO   BUMONM(IXA)
         WHEN    005
         MOVE NC"エクステリア・屋外資材"        TO   BUMONM(IXA)
         WHEN    006
         MOVE NC"作業用品"                      TO   BUMONM(IXA)
         WHEN    007
         MOVE NC"金物"                          TO   BUMONM(IXA)
         WHEN    008
         MOVE NC"工具"                          TO   BUMONM(IXA)
         WHEN    009
         MOVE NC"塗料・補修"                    TO   BUMONM(IXA)
         WHEN    010
         MOVE NC"木材・建築資材"                TO   BUMONM(IXA)
         WHEN    011
         MOVE NC"カー用品"                      TO   BUMONM(IXA)
         WHEN    012
         MOVE NC"スポーツ・玩具"                TO   BUMONM(IXA)
         WHEN    013
         MOVE NC"サイクル・レジャー"            TO   BUMONM(IXA)
         WHEN    014
         MOVE NC"ペット"                        TO   BUMONM(IXA)
         WHEN    015
         MOVE NC"日用消耗品"                    TO   BUMONM(IXA)
         WHEN    016
         MOVE NC"文具"                          TO   BUMONM(IXA)
         WHEN    017
         MOVE NC"ダイニング・キッチン"          TO   BUMONM(IXA)
         WHEN    018
         MOVE NC"バス・トイレタリー"            TO   BUMONM(IXA)
         WHEN    019
         MOVE NC"ＨＢＣ"                        TO   BUMONM(IXA)
         WHEN    020
         MOVE NC"食品・酒"                      TO   BUMONM(IXA)
         WHEN    021
         MOVE NC"インテリア"                    TO   BUMONM(IXA)
         WHEN    022
         MOVE NC"寝具"                          TO   BUMONM(IXA)
         WHEN    023
         MOVE NC"家具収納"                      TO   BUMONM(IXA)
         WHEN    024
         MOVE NC"家庭電器"                      TO   BUMONM(IXA)
         WHEN    025
         MOVE NC"冷暖房・住宅設備"              TO   BUMONM(IXA)
         WHEN    026
         MOVE NC"電材・照明"                    TO   BUMONM(IXA)
         WHEN    027
         MOVE NC"ＡＶ情報・カウンター商品"      TO   BUMONM(IXA)
         WHEN    028
         MOVE NC"テナント植物"                  TO   BUMONM(IXA)
         WHEN    029
         MOVE NC"テナントペット"                TO   BUMONM(IXA)
         WHEN    030
         MOVE NC"灯油"                          TO   BUMONM(IXA)
         WHEN    091
         MOVE NC"工事費"                        TO   BUMONM(IXA)
         WHEN    095
         MOVE NC"催事"                          TO   BUMONM(IXA)
         WHEN    OTHER
         MOVE NC"＊＊＊＊＊＊"                  TO   BUMONM(IXA)
         END-EVALUATE
*
*        小物
         MOVE      DKC-F08        TO   KOMONO(IXA)
*        異形
         MOVE      DKC-F09        TO   IKEI  (IXA)
*        ケース
         MOVE      DKC-F10        TO   KESU  (IXA)
*        他
         MOVE      DKC-F11        TO   HOKA  (IXA)
*        合計
         COMPUTE   NIGOK(IXA)  =  DKC-F08  +  DKC-F09  +
                                  DKC-F10  +  DKC-F11
*
         IF        IXA  =    1
              MOVE DKC-F07        TO   SAV-BKEY-BUMON
              MOVE DKC-F19        TO   SAV-BKEY-KBN
         END-IF
         IF        IXA  =    14
              MOVE DKC-F07        TO   SAV-NKEY-BUMON
              MOVE DKC-F19        TO   SAV-NKEY-KBN
         END-IF
*
         PERFORM   DKCNTF-READ-SUB
     END-PERFORM.
*
 BEFORE-PAGE-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     次頁処理                                  *
*----------------------------------------------------------*
 NEXT-PAGE-SUB     SECTION.
*
     PERFORM  DKCNTF-START2-SUB.
     IF       DKC-FLG   =    ZERO
              PERFORM   DKCNTF-READ-SUB
              IF   DKC-FLG   NOT =     ZERO
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
                      ( DKC-FLG   =    1  )
*        定番特売区分（名称）
         EVALUATE  DKC-F16
              WHEN    "1"
                   MOVE "1"              TO   HACCD(IXA)
                   MOVE NC"定番：送込含" TO   HACNM(IXA)
              WHEN    "2"
                   MOVE "2"              TO   HACCD(IXA)
                   MOVE NC"特売　　　　" TO   HACNM(IXA)
              WHEN    "3"
                   MOVE "3"              TO   HACCD(IXA)
                   MOVE NC"本部発注　　" TO   HACNM(IXA)
              WHEN    "4"
                   MOVE "4"              TO   HACCD(IXA)
                   MOVE NC"新店発注　　" TO   HACNM(IXA)
              WHEN    "5"
                   MOVE "5"              TO   HACCD(IXA)
                   MOVE NC"増床発注　　" TO   HACNM(IXA)
              WHEN    "6"
                   MOVE "6"              TO   HACCD(IXA)
                   MOVE NC"特注（客注）" TO   HACNM(IXA)
              WHEN    "7"
                   MOVE "7"              TO   HACCD(IXA)
                   MOVE NC"改廃　　　　" TO   HACNM(IXA)
              WHEN    "8"
                   MOVE "8"              TO   HACCD(IXA)
                   MOVE NC"商管補充　　" TO   HACNM(IXA)
              WHEN    "9"
                   MOVE "9"              TO   HACCD(IXA)
                   MOVE NC"ＢＹ改廃　　" TO   HACNM(IXA)
              WHEN    "A"
                   MOVE "A"              TO   HACCD(IXA)
                   MOVE NC"本部在庫補充" TO   HACNM(IXA)
              WHEN    "B"
                   MOVE "B"              TO   HACCD(IXA)
                   MOVE NC"備品用度品　" TO   HACNM(IXA)
              WHEN    "C"
                   MOVE "C"              TO   HACCD(IXA)
                   MOVE NC"プロモーショ" TO   HACNM(IXA)
              WHEN    OTHER
                   MOVE "*"              TO   HACCD(IXA)
                   MOVE NC"＊＊＊＊＊＊" TO   HACNM(IXA)
         END-EVALUATE
*#2019/03/18 NAV ST 発注種別変換マスタより取得
         MOVE     DKC-F03                  TO   HSB-F01
         MOVE     DKC-F19                  TO   HSB-F03
         PERFORM  DCMHSBL1-READ-SEC
         IF  DCMHSBL1-INV-FLG  =  SPACE
                  MOVE   DKC-F19           TO   HACCD(IXA)
                  MOVE   HSB-F08           TO   HACNM(IXA)
         ELSE
                   MOVE  DKC-F19           TO   HACCD(IXA)
                   MOVE  NC"＊＊＊＊＊＊"  TO   HACNM(IXA)
         END-IF
*#2019/03/18 NAV ED 発注種別変換マスタより取得
*        部門ＣＤ
         MOVE      DKC-F07        TO   BUMON (IXA)
*        部門名
         EVALUATE     DKC-F07
         WHEN    001
         MOVE NC"園芸用品・大型機械"            TO   BUMONM(IXA)
         WHEN    002
         MOVE NC"農業・業務資材"                TO   BUMONM(IXA)
         WHEN    003
         MOVE NC"用土・肥料・薬品"              TO   BUMONM(IXA)
         WHEN    004
         MOVE NC"植物"                          TO   BUMONM(IXA)
         WHEN    005
         MOVE NC"エクステリア・屋外資材"        TO   BUMONM(IXA)
         WHEN    006
         MOVE NC"作業用品"                      TO   BUMONM(IXA)
         WHEN    007
         MOVE NC"金物"                          TO   BUMONM(IXA)
         WHEN    008
         MOVE NC"工具"                          TO   BUMONM(IXA)
         WHEN    009
         MOVE NC"塗料・補修"                    TO   BUMONM(IXA)
         WHEN    010
         MOVE NC"木材・建築資材"                TO   BUMONM(IXA)
         WHEN    011
         MOVE NC"カー用品"                      TO   BUMONM(IXA)
         WHEN    012
         MOVE NC"スポーツ・玩具"                TO   BUMONM(IXA)
         WHEN    013
         MOVE NC"サイクル・レジャー"            TO   BUMONM(IXA)
         WHEN    014
         MOVE NC"ペット"                        TO   BUMONM(IXA)
         WHEN    015
         MOVE NC"日用消耗品"                    TO   BUMONM(IXA)
         WHEN    016
         MOVE NC"文具"                          TO   BUMONM(IXA)
         WHEN    017
         MOVE NC"ダイニング・キッチン"          TO   BUMONM(IXA)
         WHEN    018
         MOVE NC"バス・トイレタリー"            TO   BUMONM(IXA)
         WHEN    019
         MOVE NC"ＨＢＣ"                        TO   BUMONM(IXA)
         WHEN    020
         MOVE NC"食品・酒"                      TO   BUMONM(IXA)
         WHEN    021
         MOVE NC"インテリア"                    TO   BUMONM(IXA)
         WHEN    022
         MOVE NC"寝具"                          TO   BUMONM(IXA)
         WHEN    023
         MOVE NC"家具収納"                      TO   BUMONM(IXA)
         WHEN    024
         MOVE NC"家庭電器"                      TO   BUMONM(IXA)
         WHEN    025
         MOVE NC"冷暖房・住宅設備"              TO   BUMONM(IXA)
         WHEN    026
         MOVE NC"電材・照明"                    TO   BUMONM(IXA)
         WHEN    027
         MOVE NC"ＡＶ情報・カウンター商品"      TO   BUMONM(IXA)
         WHEN    028
         MOVE NC"テナント植物"                  TO   BUMONM(IXA)
         WHEN    029
         MOVE NC"テナントペット"                TO   BUMONM(IXA)
         WHEN    030
         MOVE NC"灯油"                          TO   BUMONM(IXA)
         WHEN    091
         MOVE NC"工事費"                        TO   BUMONM(IXA)
         WHEN    095
         MOVE NC"催事"                          TO   BUMONM(IXA)
         WHEN    OTHER
         MOVE NC"＊＊＊＊＊＊"                  TO   BUMONM(IXA)
         END-EVALUATE
*
*        小物
         MOVE      DKC-F08        TO   KOMONO(IXA)
*        異形
         MOVE      DKC-F09        TO   IKEI  (IXA)
*        ケース
         MOVE      DKC-F10        TO   KESU  (IXA)
*        他
         MOVE      DKC-F11        TO   HOKA  (IXA)
*        合計
         COMPUTE   NIGOK(IXA)  =  DKC-F08  +  DKC-F09  +
                                  DKC-F10  +  DKC-F11
*
         IF        IXA  =    1
              MOVE DKC-F07        TO   SAV-BKEY-BUMON
              MOVE DKC-F19        TO   SAV-BKEY-KBN
         END-IF
*********IF        IXA  =    14
              MOVE DKC-F07        TO   SAV-NKEY-BUMON
              MOVE DKC-F19        TO   SAV-NKEY-KBN
*********END-IF
*
         PERFORM   DKCNTF-READ-SUB
     END-PERFORM.
*
 NEXT-PAGE-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     センター納品データＳＴＡＲＴ２            *
*----------------------------------------------------------*
 DKCNTF-START2-SUB       SECTION.
*
     MOVE    ZERO            TO   DKC-FLG.
     MOVE    SPACE           TO   DKC-REC.
     INITIALIZE                   DKC-REC.
     MOVE    BDATE           TO   DKC-F01.
     MOVE    BTIME           TO   DKC-F02.
     MOVE    TOKCD           TO   DKC-F03.
     MOVE    SOKCD           TO   DKC-F04.
     MOVE    CENTCD          TO   DKC-F06.
     MOVE    NOUHIN          TO   DKC-F05.
     MOVE    SAV-NKEY-BUMON  TO   DKC-F07.
     MOVE    SAV-NKEY-KBN    TO   DKC-F19.
     START   DNCENTF     KEY  >   DKC-F01 DKC-F02 DKC-F03
                                  DKC-F04 DKC-F05 DKC-F06
                                  DKC-F19 DKC-F07
       INVALID      KEY
             MOVE       1         TO   DKC-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   DKC-FLG
     END-START.
 DKCNTF-START2-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     センター納品データＳＴＡＲＴ３            *
*----------------------------------------------------------*
 DKCNTF-START3-SUB       SECTION.
*
     MOVE    ZERO            TO   DKC-FLG.
     MOVE    SPACE           TO   DKC-REC.
     INITIALIZE                   DKC-REC.
     MOVE    BDATE           TO   DKC-F01.
     MOVE    BTIME           TO   DKC-F02.
     MOVE    TOKCD           TO   DKC-F03.
     MOVE    SOKCD           TO   DKC-F04.
     MOVE    CENTCD          TO   DKC-F06.
     MOVE    NOUHIN          TO   DKC-F05.
     MOVE    SAV-BKEY-BUMON  TO   DKC-F07.
     MOVE    SAV-BKEY-KBN    TO   DKC-F19.
     START   DNCENTF     KEY  <   DKC-F01 DKC-F02 DKC-F03
                                  DKC-F04 DKC-F05 DKC-F06
                                  DKC-F19 DKC-F07
                        WITH      REVERSED  ORDER
       INVALID      KEY
             MOVE       1         TO   DKC-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   DKC-FLG
     END-START.
 DKCNTF-START3-END.
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
*バッチ_、倉庫CD初期表示の為
     MOVE       BDATE   TO   WK-BDATE.
     MOVE       BTIME   TO   WK-BTIME.
     MOVE       TOKCD   TO   WK-TOKCD.
     MOVE       SOKCD   TO   WK-SOKCD.
     PERFORM       DKCNTF-UPD-SUB.
     PERFORM       HEADDEL-SUB.
     PERFORM       BODYDEL-SUB.
*バッチ_、倉庫CD初期表示セット
     MOVE    WK-BDATE   TO   BDATE.
     MOVE    WK-BTIME   TO   BTIME.
     MOVE    WK-TOKCD   TO   TOKCD.
     MOVE    WK-SOKCD   TO   SOKCD.
     MOVE    "1"        TO   MAIN-FLG.
*
 KAKUCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.2  センター納品データ更新                   *
*----------------------------------------------------------*
 DKCNTF-UPD-SUB          SECTION.
*    センター納品データ更新
     PERFORM  VARYING   IXA       FROM    1  BY  1
              UNTIL     IXA  >    14
*        未入力かどうかNYUMERICで判断
         IF   BUMON(IXA)     IS  NUMERIC    AND
              BUMON(IXA)     >         ZERO
**         DISPLAY "HACCD (IXA) # = " HACCD (IXA) UPON CONS
**         DISPLAY "IXA         # = " IXA         UPON CONS
              PERFORM   DKC-READ-SUB
              IF   INVALID-FLG    =    1
                   MOVE      SPACE     TO   DKC-REC
                   INITIALIZE               DKC-REC
                   MOVE BDATE          TO   DKC-F01
                   MOVE BTIME          TO   DKC-F02
                   MOVE TOKCD          TO   DKC-F03
                   MOVE SOKCD          TO   DKC-F04
                   MOVE NOUHIN         TO   DKC-F05
                   MOVE CENTCD         TO   DKC-F06
                   MOVE BUMON (IXA)    TO   DKC-F07
                   MOVE KOMONO(IXA)    TO   DKC-F08
                   MOVE IKEI  (IXA)    TO   DKC-F09
                   MOVE KESU  (IXA)    TO   DKC-F10
                   MOVE HOKA  (IXA)    TO   DKC-F11
                   MOVE HACCD (IXA)    TO   DKC-F19
                   WRITE     DKC-REC
              ELSE
                   IF   KOMONO(IXA)    NOT =  DKC-F08
                   OR   IKEI  (IXA)    NOT =  DKC-F09
                   OR   KESU  (IXA)    NOT =  DKC-F10
                   OR   HOKA  (IXA)    NOT =  DKC-F11
                        MOVE BUMON (IXA)    TO   DKC-F07
                        MOVE KOMONO(IXA)    TO   DKC-F08
                        MOVE IKEI  (IXA)    TO   DKC-F09
                        MOVE KESU  (IXA)    TO   DKC-F10
                        MOVE HOKA  (IXA)    TO   DKC-F11
                        MOVE HACCD (IXA)    TO   DKC-F19
                        REWRITE   DKC-REC
                   END-IF
              END-IF
         END-IF
     END-PERFORM.
 DKCNTF-UPD-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
*
     CLOSE    DSPF DNCENTF  ZSOKMS HTOKMS HTENMS  DCMHSBL1.
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
     MOVE   "M"     TO   EDIT-OPTION  OF  HACCD(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HACCD(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  HACNM(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HACNM(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  BUMON(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  BUMON(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  BUMONM(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  BUMONM(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  KOMONO(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  KOMONO(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  IKEI(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  IKEI(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  KESU(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  KESU(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  HOKA(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HOKA(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  NIGOK(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  NIGOK(IXB).
*
 EDIT-SET-GYO-END.
     EXIT.
*==========================================================*
*      9.3       画面表示処理                              *
*==========================================================*
 DSP-WRITE-SUB          SECTION.
*    倉庫ＣＤチェック
     MOVE     HEN-DATE        TO   SDATE.
     MOVE     HEN-TIME        TO   STIME.
     MOVE     "NSY0400I"      TO   PGID.
     MOVE     "FSY04001"      TO   FMID.
*
     IF  PARA-DSOKCD  =  "99" OR "88"
         MOVE PARA-SOKCD      TO   SOKCD
         MOVE PARA-SOKCD      TO   SOK-F01
         PERFORM ZSOKMS-READ-SEC
         IF  ZSOKMS-INV-FLG = SPACE
             MOVE SOK-F02     TO   SOKNM
         ELSE
             MOVE ALL NC"＊"  TO   SOKNM
         END-IF
         IF  PARA-DSOKCD  =  "99"
             MOVE "X"         TO   EDIT-STATUS  OF  SOKCD
         END-IF
     END-IF.
*
     MOVE    "FSY04001"       TO   DSP-FORMAT.
     MOVE    "SCREEN"         TO   DSP-GROUP.
     MOVE     SPACE           TO   DSP-PROC.
     WRITE    FSY04001.
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
*      9.7.3     センター納品データ検索                    *
*----------------------------------------------------------*
 DKC-READ-SUB           SECTION.
     MOVE    ZERO            TO   INVALID-FLG.
     MOVE    SPACE           TO   DKC-REC.
     INITIALIZE                   DKC-REC.
     MOVE    BDATE           TO   DKC-F01.
     MOVE    BTIME           TO   DKC-F02.
     MOVE    TOKCD           TO   DKC-F03.
     MOVE    SOKCD           TO   DKC-F04.
     MOVE    CENTCD          TO   DKC-F06.
     MOVE    NOUHIN          TO   DKC-F05.
     MOVE    BUMON (IXA)     TO   DKC-F07.
     MOVE    HACCD (IXA)     TO   DKC-F19.
**   DISPLAY "BUMON (IXA) = " BUMON (IXA)  UPON CONS.
**   DISPLAY "HACCD (IXA) = " HACCD (IXA)  UPON CONS.
     READ    DNCENTF
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
**   DISPLAY "INVALID-FLG = " INVALID-FLG UPON CONS.
 DKC-READ-END.
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
*#2019/03/18 NAV ST
****************************************************************
*　　発注種別変換マスタ索引
****************************************************************
 DCMHSBL1-READ-SEC         SECTION.
*
     READ     DCMHSBL1
         INVALID
           MOVE  "INV"     TO        DCMHSBL1-INV-FLG
         NOT INVALID
           MOVE  SPACE     TO        DCMHSBL1-INV-FLG
     END-READ.
*
 DCMHSBL1-READ-EXIT.
     EXIT.
*#2019/03/18 NAV ED
*****************<<  PROGRAM  END  >>***********************

```
