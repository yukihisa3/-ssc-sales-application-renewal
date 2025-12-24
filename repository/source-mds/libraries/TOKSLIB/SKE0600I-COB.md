# SKE0600I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKE0600I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタメンテ　　　　　　　　　　　*
*    モジュール名　　　　：　商品変換ＴＢＬ（出荷検品用）　　　*
*    作成日／更新日　　　：　2000/10/12                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品変換テーブルマスタの出荷検品用*
*                            項目の修正を行う。　　　　　　　　*
*                                                              *
*    修正日              ：  2008/04/23
*    修正者　            ：  NAV　佐藤
*    修正内容            ：  検品Ｇの入力で倉庫別検品取引先
*                        ：  マスタに未登録時、入力制御しない
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKE0600I.
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
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*---<<  商品変換ＴＢＬ  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL3
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F04
                                                 SHO-F02
                        FILE      STATUS    IS   SHO-STATUS.
*---<<  倉庫別検品取引先設定マスタ  >>---*
     SELECT   SOKKENF   ASSIGN    TO        DA-01-VI-SOKKENL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   KEN-F01
                                                 KEN-F02
                        FILE      STATUS    IS   KEN-STATUS.
*---<<  倉庫マスタ  >>---*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*---<<  取引先マスタ  >>---*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F012
                        FILE      STATUS    IS   MEI-STATUS.
*---<<  検品グループマスタ  >>---*
     SELECT   SOKKPGF   ASSIGN    TO        DA-01-VI-SOKKPGL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   KPG-F01
                                                 KPG-F02
                        FILE STATUS         IS   KPG-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
 01  DSP-REC            PIC  X(2000).
     COPY     FKE06001  OF        XMDLIB.
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  倉庫別検品取引先設定マスタ  >>---*
 FD  SOKKENF.
     COPY     SOKKENF   OF        XFDLIB
              JOINING   KEN       PREFIX.
*---<<  倉庫マスタ  >>---*
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*---<<  取引先マスタ  >>---*
 FD  HTOKMS.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<< 検品グループマスタ >>---*
 FD  SOKKPGF             LABEL RECORD   IS   STANDARD.
     COPY     SOKKPGF   OF        XFDLIB
              JOINING   KPG       PREFIX.
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
     02  JYO-STATUS          PIC  X(02).
     02  SHO-STATUS          PIC  X(02).
     02  KEN-STATUS          PIC  X(02).
     02  SOK-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  KPG-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  READ-FLG            PIC  X(01)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
     02  SHO-FLG             PIC  9(01)  VALUE ZERO.
     02  SOKKENF-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  ZSOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  HTOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  HMEIMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  SOKKPGF-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  WK-CHK-JANCD        PIC  X(13)  VALUE SPACE.
****  処理スイッチ  ****
 01  WK-AREA.
****  インデックス  ****
     02  IXA                 PIC  9(02).
     02  IXB                 PIC  9(02).
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
       05  SAV-BKEY-SHOCD      PIC  X(13)   VALUE  SPACE.
     03  SAV-NKEY.
       05  SAV-NKEY-SHOCD      PIC  X(13)   VALUE  SPACE.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SKE0600I".
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
            NC"仕入先マスタに存在しません".
     02  MSG-ERR3            PIC  N(20)  VALUE
             NC"入力項目に間違いがあります".
     02  MSG-ERR4            PIC  N(20) VALUE
            NC"Ｙ，Ｈ，Ｂのいずれかで入力して下さい".
     02  MSG-ERR5            PIC  N(20) VALUE
            NC"ＹかＨで入力して下さい".
     02  MSG-ERR6            PIC  N(20) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR7            PIC  N(20)  VALUE
             NC"該当データがありません".
     02  MSG-ERR8            PIC  N(20)  VALUE
             NC"前頁がありません".
     02  MSG-ERR9            PIC  N(20)  VALUE
             NC"次頁がありません".
     02  MSG-ERR10           PIC  N(20)  VALUE
             NC"自動発注区分に誤りがあります。".
     02  MSG-ERR11           PIC  N(20)  VALUE
             NC"定期区分に誤りがあります。".
     02  MSG-ERR12           PIC  N(20)  VALUE
             NC"季節区分を入力して下さい。".
     02  MSG-ERR13           PIC  N(20)  VALUE
             NC"季節区分に誤りがあります。".
     02  MSG-ERR14           PIC  N(20)  VALUE
             NC"廃盤区分に誤りがあります。".
     02  MSG-ERR15           PIC  N(20)  VALUE
             NC"仕入先マスタに未登録です。".
     02  MSG-ERR16           PIC  N(20)  VALUE
             NC"倉庫ＣＤを入力して下さい。".
     02  MSG-ERR17           PIC  N(20)  VALUE
             NC"倉庫マスタに未登録です。".
     02  MSG-ERR18           PIC  N(20)  VALUE
             NC"取引先ＣＤを入力して下さい。".
     02  MSG-ERR19           PIC  N(20)  VALUE
             NC"取引先マスタに未登録です。".
     02  MSG-ERR20           PIC  N(20)  VALUE
             NC"出荷検品対象の倉庫取引先ではありません。".
     02  MSG-ERR21           PIC  N(20)  VALUE
             NC"検品グループマスタに未登録です。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(20)
                             OCCURS 21  TIMES.
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
 FILEERR-JYO            SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE     "HJYOKEN"        TO   ERR-FL-ID.
     MOVE      JYO-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-HSHO           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE     "HSHOTBL"        TO   ERR-FL-ID.
     MOVE      SHO-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SKEN           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SOKKENF.
     MOVE     "SOKKENF"        TO   ERR-FL-ID.
     MOVE      KEN-STATUS      TO   ERR-STCD.
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
 FILEERR-HMEI           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE     "HMEIMS "        TO   ERR-FL-ID.
     MOVE      MEI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SKPG           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SOKKPGF.
     MOVE     "SOKKPGF"        TO   ERR-FL-ID.
     MOVE      KPG-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 END     DECLARATIVES.
************************************************************
 SKE0600I-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 SKE0600I-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     DISPLAY "SOKCD  = " PARA-SOKCD  UPON CONS.
     DISPLAY "DSOKCD = " PARA-DSOKCD UPON CONS.
     OPEN     I-O   DSPF    HSHOTBL.
     OPEN     INPUT HJYOKEN SOKKENF ZSOKMS HTOKMS HMEIMS SOKKPGF.
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
     MOVE      SYS-DATE2(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE2(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE2(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*特販部名称編集
     MOVE    SPACE               TO        JYO-REC.
     INITIALIZE                            JYO-REC.
     MOVE    "99"                TO        JYO-F01.
     MOVE    "BUMON"             TO        JYO-F02.
     READ    HJYOKEN
       INVALID KEY
             MOVE NC"＊＊＊＊＊＊"   TO    HEN-TOKHAN
       NOT INVALID KEY
             MOVE JYO-F03            TO    HEN-TOKHAN
     END-READ.
     MOVE    HEN-TOKHAN-AREA      TO   TOKHAN.
*
     MOVE    "1"             TO   MAIN-FLG.
*
     MOVE     SPACE          TO   FKE06001.
     PERFORM       DSP-WRITE-SUB.
     PERFORM       EDIT-SET-HEAD.
     PERFORM       EDIT-SET-BODY.
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
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
                        MOVE     SPACE TO   FKE06001
                        PERFORM             HEADDEL-SUB
                        PERFORM             BODYDEL-SUB
         WHEN   "F005"
                        MOVE     "END" TO   END-FLG
         WHEN   "E000"
                        PERFORM   EDIT-SET-HEAD
                        PERFORM   BODYDEL-SUB
                        PERFORM   HEAD-CHK-SUB
                        IF   ERR-MSG-CD     =    ZERO
                             PERFORM   SHOTBL-DSP-SUB
                             IF   ERR-MSG-CD     =    ZERO
                                  MOVE  JANCD(1) TO   WK-CHK-JANCD
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
*    倉庫ＣＤチェック
     IF SOKCD   NOT =     SPACE
        MOVE    SOKCD    TO   SOK-F01
        PERFORM ZSOKMS-READ-SEC
        IF  ZSOKMS-INV-FLG = SPACE
            MOVE SOK-F02 TO   SOKNM
        ELSE
            IF   ERR-MSG-CD     =    ZERO
                 MOVE      16   TO   ERR-MSG-CD
                 MOVE  "C" TO   EDIT-CURSOR  OF  SOKCD
            END-IF
            MOVE   "R"     TO   EDIT-OPTION  OF  SOKCD
        END-IF
     ELSE
        IF   ERR-MSG-CD     =    ZERO
             MOVE      17   TO   ERR-MSG-CD
             MOVE  "C" TO   EDIT-CURSOR  OF  SOKCD
        END-IF
        MOVE   "R"     TO   EDIT-OPTION  OF  SOKCD
     END-IF.
*    取引先チェック
     IF TOKCD   NOT   NUMERIC
     OR TOKCD   =     ZERO
        IF   ERR-MSG-CD     =    ZERO
             MOVE      18   TO   ERR-MSG-CD
             MOVE  "C" TO   EDIT-CURSOR  OF  TOKCD
        END-IF
        MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
     ELSE
        MOVE    TOKCD    TO   TOK-F01
        PERFORM HTOKMS-READ-SEC
        IF  HTOKMS-INV-FLG = SPACE
            MOVE TOK-F02 TO   TOKNM
        ELSE
            IF   ERR-MSG-CD     =    ZERO
                 MOVE      19   TO   ERR-MSG-CD
                 MOVE  "C" TO   EDIT-CURSOR  OF  TOKCD
            END-IF
            MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
        END-IF
     END-IF.
*
     IF  ERR-MSG-CD = ZERO
         MOVE    SOKCD     TO   KEN-F01
         MOVE    TOKCD     TO   KEN-F02
         PERFORM SOKKENF-READ-SEC
***2008/04/23 STA
*        IF  SOKKENF-INV-FLG = "INV"
*           IF   ERR-MSG-CD     =    ZERO
*                MOVE      20   TO   ERR-MSG-CD
*                MOVE  "C" TO   EDIT-CURSOR  OF  SOKCD
*           END-IF
*           MOVE   "R"     TO   EDIT-OPTION  OF  SOKCD
*           MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
***2008/04/23 END
     END-IF.
*
 HEAD-CHK-EXIT.
     EXIT.
*==========================================================*
*      2.1.1     明細セット処理                            *
*==========================================================*
 SHOTBL-DSP-SUB       SECTION.
*
     PERFORM  EDIT-SET-BODY.
     MOVE     ZERO      TO   ERR-MSG-CD.
*
     PERFORM  SHOTBL-START-SUB.
     IF       SHO-FLG   =    ZERO
              PERFORM   SHOTBL-READ-SUB
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      7    TO   ERR-MSG-CD
              END-IF
              MOVE   "R"     TO   EDIT-OPTION  OF  TSYOCD
              GO      TO     SHOTBL-DSP-EXIT
     END-IF.
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL   ( IXA       >    7 )  OR
                      ( SHO-FLG   =    1 )
*        相手商品ＣＤ
         MOVE      SHO-F02        TO   JANCD (IXA)
*        商品ＣＤ
         MOVE      SHO-F031       TO   SHOCD (IXA)
*        品単ＣＤ
         MOVE      SHO-F0321      TO   MHIN1 (IXA)
         MOVE      SHO-F0322      TO   MHIN2 (IXA)
         MOVE      SHO-F0323      TO   MHIN3 (IXA)
*        商品名称
         MOVE      SHO-F031       TO   MEI-F011
         MOVE      SHO-F032       TO   MEI-F012
         PERFORM HMEIMS-READ-SEC
         IF  HMEIMS-INV-FLG  =  SPACE
             MOVE  MEI-F02        TO   HINMEI(IXA)
         ELSE
             MOVE  ALL NC"＊"     TO   HINMEI(IXA)
         END-IF
*        _番
         MOVE      SHO-F08        TO   TANABN(IXA)
*        分類
         MOVE      SHO-F07        TO   BUNRUI(IXA)
*        検品グループコード
         MOVE      SHO-F11        TO   KENPGR(IXA)
*
         IF        IXA  =    1
              MOVE SHO-F02        TO   SAV-BKEY-SHOCD
         END-IF
         IF        IXA  =    7
              MOVE SHO-F02        TO   SAV-NKEY-SHOCD
         END-IF
*
         PERFORM   SHOTBL-READ-SUB
     END-PERFORM.
*
 SHOTBL-DSP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.1.1.1   商品変換ＴＢＬＳＴＡＲＴ                  *
*----------------------------------------------------------*
 SHOTBL-START-SUB        SECTION.
*
     MOVE    ZERO            TO   SHO-FLG.
     MOVE    SPACE           TO   SHO-REC.
     INITIALIZE                   SHO-REC.
     MOVE    TOKCD           TO   SHO-F01.
     MOVE    SOKCD           TO   SHO-F04.
     MOVE    TSYOCD          TO   SHO-F02.
     START   HSHOTBL     KEY  >=  SHO-F01 SHO-F04 SHO-F02
       INVALID      KEY
             MOVE       1         TO   SHO-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   SHO-FLG
     END-START.
 SHOTBL-START-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.1.2   商品名称マスタＲＥＡＤ                    *
*----------------------------------------------------------*
 SHOTBL-READ-SUB        SECTION.
*
     READ    HSHOTBL    NEXT
         AT END
             MOVE      1     TO   SHO-FLG
             GO              TO   SHOTBL-READ-END
         NOT AT END
             MOVE      ZERO  TO   SHO-FLG
     END-READ.
*
     IF      SHO-F01  NOT =  TOKCD
     OR      SHO-F04  NOT =  SOKCD
     OR      WK-CHK-JANCD >  SHO-F02
             MOVE      1     TO   SHO-FLG
     END-IF.
*
 SHOTBL-READ-END.
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
              UNTIL    IXB   >     7
         IF   JANCD(IXB)   =   SPACE
              MOVE   "X"   TO   EDIT-STATUS  OF  JANCD (IXB)
         END-IF
     END-PERFORM.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE     SPACE TO   FKE06001
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
                            PERFORM   SHOTBL-UPD-SUB
                            PERFORM   BEFORE-PAGE-SUB
                        END-IF
         WHEN   "F012"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   SHOTBL-UPD-SUB
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
              UNTIL     IXA    >   7
         IF   JANCD (IXA) =  LOW-VALUE
              MOVE   SPACE      TO   SHOCD(IXA)
         END-IF
*        検品ＧＣＤチェック
         IF  KENPGR(IXA)    NUMERIC
         AND KENPGR(IXA)    NOT = ZERO
             MOVE  SOKCD    TO   KPG-F01
             MOVE  KENPGR(IXA)   TO   KPG-F02
             PERFORM SOKKPGF-READ-SEC
***2008/04/23 STA
             IF  SOKKENF-INV-FLG = SPACE
***2008/04/23 END
                IF  SOKKPGF-INV-FLG = "INV"
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE      21   TO   ERR-MSG-CD
                        MOVE "C" TO  EDIT-CURSOR  OF  KENPGR(IXA)
                        END-IF
                        MOVE "R" TO  EDIT-OPTION  OF  KENPGR(IXA)
                   END-IF
                 END-IF
              END-IF
     END-PERFORM.
*
 BODYCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.2     前頁処理                                  *
*----------------------------------------------------------*
 BEFORE-PAGE-SUB     SECTION.
*
     PERFORM  SHOTBL-START3-SUB.
     IF       SHO-FLG   =    ZERO
              PERFORM   SHOTBL-READ-SUB
              IF   SHO-FLG   NOT =     ZERO
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE      8    TO   ERR-MSG-CD
                   END-IF
                   GO   TO   BEFORE-PAGE-EXIT
              END-IF
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      8    TO   ERR-MSG-CD
              END-IF
              GO   TO   BEFORE-PAGE-EXIT
     END-IF.
     MOVE     SPACE     TO   SAV-BKEY.
     PERFORM  BODYDEL-SUB.
     PERFORM  VARYING   IXA  FROM  7  BY  -1
              UNTIL   ( IXA       =    0 )  OR
                      ( SHO-FLG   =    1 )
*        相手商品ＣＤ
         MOVE      SHO-F02        TO   JANCD (IXA)
*        商品ＣＤ
         MOVE      SHO-F031       TO   SHOCD (IXA)
*        品単ＣＤ
         MOVE      SHO-F0321      TO   MHIN1 (IXA)
         MOVE      SHO-F0322      TO   MHIN2 (IXA)
         MOVE      SHO-F0323      TO   MHIN3 (IXA)
*        商品名称
         MOVE      SHO-F031       TO   MEI-F011
         MOVE      SHO-F032       TO   MEI-F012
         PERFORM HMEIMS-READ-SEC
         IF  HMEIMS-INV-FLG  =  SPACE
             MOVE  MEI-F02        TO   HINMEI(IXA)
         ELSE
             MOVE  ALL NC"＊"     TO   HINMEI(IXA)
         END-IF
*        _番
         MOVE      SHO-F08        TO   TANABN(IXA)
*        分類
         MOVE      SHO-F07        TO   BUNRUI(IXA)
*        検品グループコード
         MOVE      SHO-F11        TO   KENPGR(IXA)
*
         IF        IXA  =    1
              MOVE SHO-F02        TO   SAV-BKEY-SHOCD
         END-IF
         IF        IXA  =    7
              MOVE SHO-F02        TO   SAV-NKEY-SHOCD
         END-IF
*
         PERFORM   SHOTBL-READ-SUB
     END-PERFORM.
*
 BEFORE-PAGE-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     次頁処理                                  *
*----------------------------------------------------------*
 NEXT-PAGE-SUB     SECTION.
*
     PERFORM  SHOTBL-START2-SUB.
     IF       SHO-FLG   =    ZERO
              PERFORM   SHOTBL-READ-SUB
              IF   SHO-FLG   NOT =     ZERO
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE      9    TO   ERR-MSG-CD
                   END-IF
                   GO   TO   NEXT-PAGE-EXIT
              END-IF
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      9    TO   ERR-MSG-CD
              END-IF
              GO   TO   NEXT-PAGE-EXIT
     END-IF.
     MOVE     SPACE     TO   SAV-NKEY.
     PERFORM  BODYDEL-SUB.
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL   ( IXA       >    7 )  OR
                      ( SHO-FLG   =    1 )
*        相手商品ＣＤ
         MOVE      SHO-F02        TO   JANCD (IXA)
*        商品ＣＤ
         MOVE      SHO-F031       TO   SHOCD (IXA)
*        品単ＣＤ
         MOVE      SHO-F0321      TO   MHIN1 (IXA)
         MOVE      SHO-F0322      TO   MHIN2 (IXA)
         MOVE      SHO-F0323      TO   MHIN3 (IXA)
*        商品名称
         MOVE      SHO-F031       TO   MEI-F011
         MOVE      SHO-F032       TO   MEI-F012
         PERFORM HMEIMS-READ-SEC
         IF  HMEIMS-INV-FLG  =  SPACE
             MOVE  MEI-F02        TO   HINMEI(IXA)
         ELSE
             MOVE  ALL NC"＊"     TO   HINMEI(IXA)
         END-IF
*        _番
         MOVE      SHO-F08        TO   TANABN(IXA)
*        分類
         MOVE      SHO-F07        TO   BUNRUI(IXA)
*        検品グループコード
         MOVE      SHO-F11        TO   KENPGR(IXA)
*
         IF        IXA  =    1
              MOVE SHO-F02        TO   SAV-BKEY-SHOCD
         END-IF
         IF        IXA  =    7
              MOVE SHO-F02        TO   SAV-NKEY-SHOCD
         END-IF
*
         PERFORM   SHOTBL-READ-SUB
     END-PERFORM.
*
 NEXT-PAGE-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     商品名称マスタＳＴＡＲＴ２                *
*----------------------------------------------------------*
 SHOTBL-START2-SUB       SECTION.
*
     MOVE    ZERO            TO   SHO-FLG.
     MOVE    SPACE           TO   SHO-REC.
     INITIALIZE                   SHO-REC.
     MOVE    TOKCD           TO   SHO-F01.
     MOVE    SOKCD           TO   SHO-F04.
     MOVE    SAV-NKEY-SHOCD  TO   SHO-F02.
     START   HSHOTBL     KEY  >   SHO-F01 SHO-F04 SHO-F02
       INVALID      KEY
             MOVE       1         TO   SHO-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   SHO-FLG
     END-START.
 SHOTBL-START2-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.4     商品名称マスタＳＴＡＲＴ３                *
*----------------------------------------------------------*
 SHOTBL-START3-SUB       SECTION.
*
     MOVE    ZERO            TO   SHO-FLG.
     MOVE    SPACE           TO   SHO-REC.
     INITIALIZE                   SHO-REC.
     MOVE    TOKCD           TO   SHO-F01.
     MOVE    SOKCD           TO   SHO-F04.
     MOVE    SAV-BKEY-SHOCD  TO   SHO-F02.
     START   HSHOTBL     KEY  <   SHO-F01 SHO-F04 SHO-F02
                        WITH      REVERSED  ORDER
       INVALID      KEY
             MOVE       1         TO   SHO-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   SHO-FLG
     END-START.
 SHOTBL-START3-END.
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
     PERFORM       SHOTBL-UPD-SUB.
     PERFORM       HEADDEL-SUB.
     PERFORM       BODYDEL-SUB.
     MOVE    "1"   TO  MAIN-FLG.
*
 KAKUCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.2  商品変換ＴＢＬ更新                       *
*----------------------------------------------------------*
 SHOTBL-UPD-SUB          SECTION.
*    商品名称マスタ更新
     PERFORM  VARYING   IXA       FROM    1  BY  1
              UNTIL     IXA  >    7
         IF   SHOCD(IXA)     NOT =     SPACE
              PERFORM   SHO-READ-SUB
              IF   INVALID-FLG    =    1
                   CONTINUE
              ELSE
                   IF ( KENPGR(IXA)    NOT =     SHO-F11 )
                        MOVE KENPGR(IXA)    TO   SHO-F11
                        MOVE SYS-DATE2      TO   SHO-F99
                        REWRITE   SHO-REC
                   END-IF
              END-IF
         END-IF
     END-PERFORM.
 SHOTBL-UPD-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF HJYOKEN HSHOTBL SOKKENF ZSOKMS HTOKMS HMEIMS
              SOKKPGF.
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
     MOVE   "M"     TO   EDIT-OPTION  OF  SOKCD.
*****MOVE   SPACE   TO   EDIT-CURSOR  OF  SOKCD.
     MOVE   "M"     TO   EDIT-OPTION  OF  TOKCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TOKCD.
     MOVE   "M"     TO   EDIT-OPTION  OF  TSYOCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TSYOCD.
 EDIT-SET-HEAD-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.3     項目制御部初期化（ボディ部）              *
*----------------------------------------------------------*
 EDIT-SET-BODY          SECTION.
     PERFORM  VARYING IXB     FROM  1  BY  1
              UNTIL   IXB     >     7
         PERFORM     EDIT-SET-GYO
     END-PERFORM.
 EDIT-SET-BODY-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.3.1   項目制御部初期化（ボディ部１行）          *
*----------------------------------------------------------*
 EDIT-SET-GYO           SECTION.
*
     MOVE   "M"     TO   EDIT-OPTION  OF  KENPGR(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  KENPGR(IXB).
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
              MOVE PARA-SOKCD  TO   SOK-F01 SOKCD
              PERFORM ZSOKMS-READ-SEC
              IF  ZSOKMS-INV-FLG = SPACE
                  MOVE SOK-F02 TO   SOKNM
              ELSE
                  MOVE ALL NC"＊" TO SOKNM
              END-IF
              MOVE "X"     TO   EDIT-STATUS OF SOKCD
     END-IF.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
     MOVE     HEN-TOKHAN-AREA     TO   TOKHAN.
     MOVE    "FKE06001"      TO   DSP-FORMAT.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     WRITE    FKE06001.
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
     MOVE  SPACE        TO   MAS002.
     PERFORM            EDIT-SET-BODY.
 BODYDEL-END.
     EXIT.
*==========================================================*
*      9.7       ファイル処理                              *
*==========================================================*
*----------------------------------------------------------*
*      9.7.1     条件ファイルＲＥＡＤ                      *
*----------------------------------------------------------*
 JYO-READ-SUB           SECTION.
     READ    HJYOKEN
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
          INITIALIZE              JYO-REC
       NOT INVALID  KEY
          MOVE      ZERO     TO   INVALID-FLG
     END-READ.
 JYO-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.3     商品名称マスタの検索                      *
*----------------------------------------------------------*
 SHO-READ-SUB           SECTION.
     MOVE    TOKCD           TO   SHO-F01.
     MOVE    SOKCD           TO   SHO-F04.
     MOVE    JANCD (IXA)     TO   SHO-F02.
     READ    HSHOTBL
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 SHO-READ-END.
     EXIT.
*----------------------------------------------------------*
*                倉庫別検品取引先設定マスタ                *
*----------------------------------------------------------*
 SOKKENF-READ-SEC       SECTION.
*
     READ    SOKKENF
       INVALID      KEY
          MOVE     "INV"     TO   SOKKENF-INV-FLG
       NOT INVALID  KEY
          MOVE      SPACE    TO   SOKKENF-INV-FLG
     END-READ.
*
 SOKKENF-READ-EXIT.
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
*                商品名称マスタ                            *
*----------------------------------------------------------*
 HMEIMS-READ-SEC       SECTION.
*
     READ    HMEIMS
       INVALID      KEY
          MOVE     "INV"     TO   HMEIMS-INV-FLG
       NOT INVALID  KEY
          MOVE      SPACE    TO   HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*                検品グループマスタ                        *
*----------------------------------------------------------*
 SOKKPGF-READ-SEC       SECTION.
*
     READ    SOKKPGF
       INVALID      KEY
          MOVE     "INV"     TO   SOKKPGF-INV-FLG
       NOT INVALID  KEY
          MOVE      SPACE    TO   SOKKPGF-INV-FLG
     END-READ.
*
 SOKKPGF-READ-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
