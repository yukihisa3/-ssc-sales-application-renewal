****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタメンテ　　　　　　　　　　　*
*    モジュール名　　　　：　商品変換ＴＢＬ（出荷検品用）　　　*
*    作成日／更新日　　　：　2000/10/12                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品変換テーブルマスタの項目修正を*
*                            行なう。　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SMNT043.
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
*---<<  商品変換履歴ファイル>>---*
     SELECT   HSHOTBR   ASSIGN    TO        DA-01-VI-HSHOTBR1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   STR-F01
                                                 STR-F02
                                                 STR-F03
                        STATUS              IS   STR-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
 01  DSP-REC            PIC  X(2000).
     COPY     FM043  OF        XMDLIB.
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
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
*---<< 商品変換テーブル履歴ファイル >>
 FD  HSHOTBR.
     COPY     HSHOTBR   OF        XFDLIB
              JOINING   STR       PREFIX.
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
     02  SOK-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  STR-STATUS          PIC  X(02).
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SMNT043".
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
             NC"倉庫マスタに未入力です。".
     02  MSG-ERR3            PIC  N(20)  VALUE
             NC"倉庫ＣＤを入力して下さい。".
     02  MSG-ERR4            PIC  N(20)  VALUE
             NC"取引先マスタに未登録です。".
     02  MSG-ERR5            PIC  N(20)  VALUE
             NC"取引先ＣＤを入力して下さい。".
     02  MSG-ERR6            PIC  N(20)  VALUE
             NC"商品変換テーブルに未登録です。".
     02  MSG-ERR7            PIC  N(20)  VALUE
             NC"商品名称マスタに未登録です。".
     02  MSG-ERR8            PIC  N(20)  VALUE
             NC"地域区分を入力して下さい。".
     02  MSG-ERR9            PIC  N(20)  VALUE
             NC"地域区分と正しく入力して下さい。".
     02  MSG-ERR10           PIC  N(20)  VALUE
             NC"前頁がありません。".
     02  MSG-ERR11           PIC  N(20)  VALUE
             NC"次頁がありません。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(20)
                             OCCURS 11  TIMES.
*レコード格納領域
     COPY   HSHOTBL  OF XFDLIB  JOINING   TBL  AS   PREFIX.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
 LINKAGE                SECTION.
 01  PARA-SOKCD            PIC X(02).
 01  PARA-DSOKCD           PIC X(02).
 01  PARA-TAN              PIC X(02).
 01  PARA-BUMON            PIC X(04).
 01  PARA-NYURYOKUHI       PIC 9(08).
 01  PARA-TIME             PIC 9(04).
*----------------------------------------------------------*
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
*----------------------------------------------------------*
 PROCEDURE              DIVISION  USING  PARA-SOKCD PARA-DSOKCD
              PARA-TAN  PARA-BUMON  PARA-NYURYOKUHI PARA-TIME.
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
 FILEERR-HSHOTBR        SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBR.
     MOVE     "HSHOTBR"        TO   ERR-FL-ID.
     MOVE      STR-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 END     DECLARATIVES.
************************************************************
 SMNT043-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 SMNT043-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O   DSPF    HSHOTBL  HSHOTBR.
     OPEN     INPUT HJYOKEN ZSOKMS HTOKMS HMEIMS.
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
     MOVE      LINK-OUT-YMD       TO   DATE-AREA PARA-NYURYOKUHI.
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
*履歴発行用
     MOVE      WK-TIME(1:4)       TO   PARA-TIME.
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
     MOVE     SPACE          TO   FM043.
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
                        MOVE     SPACE TO   FM043
                        PERFORM             HEADDEL-SUB
                        PERFORM             BODYDEL-SUB
         WHEN   "F005"
                        MOVE     "END" TO   END-FLG
         WHEN   "E000"
                        PERFORM   EDIT-SET-HEAD
                        PERFORM   BODYDEL-SUB
                        PERFORM   HEAD-CHK-SUB
                        IF   ERR-MSG-CD     =    ZERO
                             MOVE SPACE     TO   WK-CHK-JANCD
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
*    取引先チェック
     IF TOKCD   NOT   NUMERIC
     OR TOKCD   =     ZERO
        IF   ERR-MSG-CD     =    ZERO
             MOVE      05   TO   ERR-MSG-CD
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
                 MOVE      04   TO   ERR-MSG-CD
                 MOVE  "C" TO   EDIT-CURSOR  OF  TOKCD
            END-IF
            MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
        END-IF
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
     MOVE     ZERO      TO   SHO-FLG.
     PERFORM  SHOTBL-START-SUB.
     IF       SHO-FLG   =    ZERO
              PERFORM   SHOTBL-READ-SUB
**************DISPLAY "AAA" UPON CONS
**************DISPLAY "SHO-F01 = " SHO-F01 UPON CONS
              IF  SHO-FLG = 1
***************DISPLAY "BBB" UPON CONS
                  IF   ERR-MSG-CD     =    ZERO
                       MOVE     06    TO   ERR-MSG-CD
                  END-IF
                  MOVE   "C"     TO   EDIT-CURSOR  OF  SOKCD
                  MOVE   "R"     TO   EDIT-OPTION  OF  SOKCD
                  MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
                  MOVE   "R"     TO   EDIT-OPTION  OF  TSYOCD
                  GO      TO     SHOTBL-DSP-EXIT
              END-IF
     ELSE
***************DISPLAY "CCC" UPON CONS
              IF   ERR-MSG-CD     =    ZERO
                   MOVE     06    TO   ERR-MSG-CD
              END-IF
              MOVE   "C"     TO   EDIT-CURSOR  OF  SOKCD
              MOVE   "R"     TO   EDIT-OPTION  OF  SOKCD
              MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
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
*        原価単価
         MOVE      SHO-F05        TO   GENTAN(IXA)
*        売価単価
         MOVE      SHO-F06        TO   BAITAN(IXA)
*        仕入単価
         MOVE      SHO-F09        TO   SIRTAN(IXA)
*        地域区分
*********MOVE      SHO-F12        TO   TIIKI(IXA)
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
              MOVE   "X"   TO   EDIT-STATUS  OF  SHOCD (IXB)
              MOVE   "X"   TO   EDIT-STATUS  OF  MHIN1 (IXB)
              MOVE   "X"   TO   EDIT-STATUS  OF  MHIN2 (IXB)
              MOVE   "X"   TO   EDIT-STATUS  OF  MHIN3 (IXB)
              MOVE   "X"   TO   EDIT-STATUS  OF  TANABN(IXB)
              MOVE   "X"   TO   EDIT-STATUS  OF  BUNRUI(IXB)
              MOVE   "X"   TO   EDIT-STATUS  OF  GENTAN(IXB)
              MOVE   "X"   TO   EDIT-STATUS  OF  BAITAN(IXB)
              MOVE   "X"   TO   EDIT-STATUS  OF  SIRTAN(IXB)
              MOVE   "X"   TO   EDIT-STATUS  OF  TIIKI (IXB)
         END-IF
     END-PERFORM.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE     SPACE TO   FM043
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
*        ＢＯＤＹ部チェック
         IF   JANCD (IXA)  NOT =  SPACE
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
*    自社商品ＣＤ
     MOVE      SHOCD(IXA)     TO   MEI-F011.
*    品単ＣＤ
     MOVE      MHIN1(IXA)     TO   MEI-F0121.
     MOVE      MHIN2(IXA)     TO   MEI-F0122.
     MOVE      MHIN3(IXA)     TO   MEI-F0123.
     PERFORM HMEIMS-READ-SEC
     IF  HMEIMS-INV-FLG  = "INV"
             IF  ERR-MSG-CD   =    ZERO
                 MOVE  07  TO   ERR-MSG-CD
                 MOVE  "C" TO   EDIT-CURSOR  OF  SHOCD(IXA)
             END-IF
             MOVE   "R"    TO   EDIT-OPTION  OF  SHOCD(IXA)
             MOVE   "R"    TO   EDIT-OPTION  OF  MHIN1(IXA)
             MOVE   "R"    TO   EDIT-OPTION  OF  MHIN2(IXA)
             MOVE   "R"    TO   EDIT-OPTION  OF  MHIN3(IXA)
     ELSE
             MOVE  MEI-F02 TO   HINMEI(IXA)
     END-IF.
*    地域区分チェック
*    IF      TIIKI(IXA)  =  SPACE
*            IF  ERR-MSG-CD   =    ZERO
*                MOVE  08  TO   ERR-MSG-CD
*                MOVE  "C" TO   EDIT-CURSOR  OF  TIIKI(IXA)
*            END-IF
*            MOVE   "R"    TO   EDIT-OPTION  OF  TIIKI(IXA)
*    ELSE
*            IF  TIIKI(IXA) = "1" OR "2" OR "3" OR "4" OR "5" OR
*                             "6" OR "7" OR "8" OR "9"
*                CONTINUE
*            ELSE
*                IF  ERR-MSG-CD   =    ZERO
*                    MOVE  09  TO   ERR-MSG-CD
*                    MOVE  "C" TO   EDIT-CURSOR  OF  TIIKI(IXA)
*                END-IF
*                MOVE   "R"    TO   EDIT-OPTION  OF  TIIKI(IXA)
*            END-IF
*    END-IF.
*    単価チェック
     IF      SIRTAN(IXA)  <=  GENTAN(IXA)
     AND     GENTAN(IXA)  <=  BAITAN(IXA)
             MOVE  SPACE   TO   MSSG(IXA)
     ELSE
             MOVE  NC"原"  TO   MSSG(IXA)
     END-IF.
*
 BODYCHK1-END.
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
*        原価単価
         MOVE      SHO-F05        TO   GENTAN(IXA)
*        売価単価
         MOVE      SHO-F06        TO   BAITAN(IXA)
*        仕入単価
         MOVE      SHO-F09        TO   SIRTAN(IXA)
*        地域区分
*********MOVE      SHO-F12        TO   TIIKI(IXA)
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
*        原価単価
         MOVE      SHO-F05        TO   GENTAN(IXA)
*        売価単価
         MOVE      SHO-F06        TO   BAITAN(IXA)
*        仕入単価
         MOVE      SHO-F09        TO   SIRTAN(IXA)
*        地域区分
*********MOVE      SHO-F12        TO   TIIKI(IXA)
*
         IF        IXA  =    1
              MOVE SHO-F02        TO   SAV-BKEY-SHOCD
         END-IF
*********IF        IXA  =    7
              MOVE SHO-F02        TO   SAV-NKEY-SHOCD
*********END-IF
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
                   IF   SHOCD(IXA)  NOT =  SHO-F031
                   OR   MHIN1(IXA)  NOT =  SHO-F0321
                   OR   MHIN2(IXA)  NOT =  SHO-F0322
                   OR   MHIN3(IXA)  NOT =  SHO-F0323
                   OR   TANABN(IXA) NOT =  SHO-F08
                   OR   BUNRUI(IXA) NOT =  SHO-F07
                   OR   GENTAN(IXA) NOT =  SHO-F05
                   OR   BAITAN(IXA) NOT =  SHO-F06
                   OR   SIRTAN(IXA) NOT =  SHO-F09
*******************OR   TIIKI(IXA)  NOT =  SHO-F12
                        MOVE SHO-REC      TO   STR-REC(16:85)
                        MOVE DATE-AREA    TO   STR-F01
                        MOVE PARA-TIME    TO   STR-F02
                        MOVE PARA-TAN     TO   STR-F03
                        MOVE "2"          TO   STR-F04
                        MOVE SHOCD(IXA)   TO   SHO-F031
                        MOVE MHIN1(IXA)   TO   SHO-F0321
                        MOVE MHIN2(IXA)   TO   SHO-F0322
                        MOVE MHIN3(IXA)   TO   SHO-F0323
                        MOVE TANABN(IXA)  TO   SHO-F08
                        MOVE BUNRUI(IXA)  TO   SHO-F07
                        MOVE GENTAN(IXA)  TO   SHO-F05
                        MOVE BAITAN(IXA)  TO   SHO-F06
                        MOVE SIRTAN(IXA)  TO   SHO-F09
************************MOVE TIIKI(IXA)   TO   SHO-F12
                        MOVE PARA-TAN     TO   SHO-F14
                        MOVE SYS-DATE2    TO   SHO-F99
                        MOVE SHO-REC      TO   STR-REC(101:85)
                        WRITE     STR-REC
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
*
     CLOSE    DSPF HJYOKEN HSHOTBL  ZSOKMS HTOKMS HMEIMS HSHOTBR.
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
     MOVE   "M"     TO   EDIT-OPTION  OF  SHOCD(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SHOCD(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  MHIN1(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  MHIN1(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  MHIN2(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  MHIN2(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  MHIN3(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  MHIN3(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  TANABN(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TANABN(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  BUNRUI(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  BUNRUI(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  GENTAN(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  GENTAN(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  BAITAN(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  BAITAN(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  SIRTAN(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SIRTAN(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  TIIKI(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TIIKI(IXB).
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
     MOVE     HEN-DATE        TO   SDATE.
     MOVE     HEN-TIME        TO   STIME.
     MOVE     HEN-TOKHAN-AREA TO   TOKHAN.
     MOVE    "FM043"          TO   DSP-FORMAT.
     MOVE    "SCREEN"         TO   DSP-GROUP.
     MOVE     SPACE           TO   DSP-PROC.
     WRITE    FM043.
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
*****************<<  PROGRAM  END  >>***********************
