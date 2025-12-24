# SIT0150I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SIT0150I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　倉庫マスタメンテナンス　　　　　　*
*    作成日／更新日　　　：　09/03/24                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　倉庫マスタの登録・修正・削除　　　*
*                            を行う                            *
*                           （ＩＴ統制対応）ZMT0050O改修 　*
*    更新日　　　　　　　：　97/03/11                          *
*    　　　　　　　　　　：　運転手段の追加　　　　　　　　　　*
*    更新日　　　　　　　：　11/04/14                          *
*    　　　　　　　　　　：　出荷送信の追加（オンライン）追加　*
*    更新日　　　　　　　：　11/05/31                          *
*    　　　　　　　　　　：　出荷送信（手書）追加　　　　　　　*
*    更新日　　　　　　　：　12/07/24                          *
*    　　　　　　　　　　：　小売連携区分　追加　　　　　　　　*
*    更新日　　　　　　　：　12/10/09                          *
*    　　　　　　　　　　：　物流連携区分　追加　　　　　　　　*
*    更新日／更新者　　　：　19/03/22  /INOUE                  *
*             S11300660 マスタ更新履歴ファイル項目追加         *
*    更新日／更新者　　　：　20/02/12  /T.TAKAHASHI            *
*    　　　　　　　　　　：　倉庫区分追加（Ｄ３６５対応）      *
*    更新日／更新者　　　：　20/05/14  /T.TAKAHASHI            *
*    　　　　　　　　　　：　Ｄ３６５計上用項目追加　　）      *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SIT0150I.
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
                        FILE      STATUS    IS   DSP-STATUS.
*---<<  倉庫マスタ  >>---*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*マスタ更新履歴ファイル
     SELECT     MSTLOGF    ASSIGN    TO        DA-01-VI-MSTLOGL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       MSL-F01
                                               MSL-F02
                                               MSL-F03
                                               MSL-F04
                                               MSL-F05
                                               MSL-F06
                                               MSL-F07
                           FILE      STATUS    MSL-STATUS.
*# 2020/05/14 NAV ST
*---<<  条件Ｆ　　  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01  JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*# 2020/05/14 NAV ED
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
     COPY     FIT01501  OF        XMDLIB
              JOINING   DSP       PREFIX.
*---<<  倉庫マスタ  >>---*
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*マスタ更新履歴ファイル
 FD  MSTLOGF.
     COPY     MSTLOGF   OF        XFDLIB
              JOINING   MSL       PREFIX.
*# 2020/05/14 NAV ST
*---<<  条件Ｆ　　  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*# 2020/05/14 NAV ED
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  画面制御項目  ***
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  SOK-STATUS          PIC  X(02).
     02  MSL-STATUS          PIC  X(02).
*# 2020/05/14 NAV ST
     02  JYO-STATUS          PIC  X(02).
*# 2020/05/14 NAV ED
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
*# 2020/05/14 NAV ST
     02  HJYOKEN-INV-FLG     PIC  X(03)  VALUE SPACE.
*# 2020/05/14 NAV ED
 01  WK-AREA.
     02  WK-SYORI            PIC  9(01)  VALUE ZERO.
*
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
*
 01  MSG-AREA.
     02  PMSG01            PIC N(20) VALUE
                           NC"_取消".
     02  PMSG02            PIC N(20) VALUE
                           NC"_取消　_再入力".
     02  PMSG03            PIC N(20) VALUE
                           NC"_終了".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SIT0150I".
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
     02  MSG-ERR1            PIC  N(28)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(28)  VALUE
            NC"既に登録済です。".
     02  MSG-ERR3            PIC  N(28)  VALUE
            NC"倉庫コードが未登録です。".
     02  MSG-ERR4            PIC  N(28)  VALUE
            NC"倉庫コードが未入力です。".
     02  MSG-ERR5            PIC  N(28) VALUE
            NC"処理区分が違います".
     02  MSG-ERR6            PIC  N(28) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR7            PIC  N(28) VALUE
            NC"倉庫名が未入力です".
     02  MSG-ERR8            PIC  N(28) VALUE
            NC"運転手段が違います".
     02  MSG-ERR9            PIC  N(28) VALUE
            NC"出荷送信区分が違います".
***** 2012/07/24 ADD    ST 小売連携区分追加に伴うメッセージ追加
     02  MSG-ERR10           PIC  N(28) VALUE
            NC"小売連携区分が違います".
***** 2012/07/24 ADD    ED 小売連携区分追加に伴うメッセージ追加
***** 2012/10/09 ADD    ST 物流連携区分追加に伴うメッセージ追加
     02  MSG-ERR11           PIC  N(28) VALUE
            NC"物流連携区分が違います".
***** 2012/10/09 ADD    ED 物流連携区分追加に伴うメッセージ追加
***** 2020/02/12 ADD    ST 倉庫区分追加に伴うメッセージ追加
     02  MSG-ERR12           PIC  N(28) VALUE
            NC"倉庫区分が違います".
***** 2020/02/12 ADD    ED 倉庫区分追加に伴うメッセージ追加
***** 2020/05/14 ADD    ST
     02  MSG-ERR13           PIC  N(28) VALUE
            NC"条件Ｆ未登録（サイト）".
     02  MSG-ERR14           PIC  N(28) VALUE
            NC"条件Ｆ未登録（保管場所）".
***** 2020/05/14 ADD    ED
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
***** 2012/07/24 ADD    ST 小売連携区分追加に伴うメッセージ追加
*****************************OCCURS  9   TIMES.
*****************************OCCURS  10  TIMES.
***** 2012/07/24 ADD    ED 小売連携区分追加に伴うメッセージ追加
***** 2012/10/09 ADD    ST 物流連携区分追加に伴うメッセージ追加
*****************************OCCURS  11  TIMES.
***** 2012/10/09 ADD    ED 物流連携区分追加に伴うメッセージ追加
***** 2020/02/12 ADD    ST 倉庫区分追加に伴うメッセージ追加
*****************************OCCURS  12  TIMES.
***** 2020/02/12 ADD    ED 倉庫区分追加に伴うメッセージ追加
***** 2020/05/14 ADD    ST
                             OCCURS  14  TIMES.
***** 2020/05/14 ADD    ED
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*レコード退避エリア
*↓2019.03.22
 01  BEFORE-DATA           PIC  X(200).
*↑2019.03.22
 01  DATA-TAIHI            PIC  X(200).
*
 01  SEQ                   PIC  9(02).
*
 LINKAGE                   SECTION.
 01  PARA-BUMONCD          PIC X(04).
 01  PARA-TANCD            PIC X(08).
 01  PARA-UPDTDATE         PIC 9(08).
 01  PARA-UPDTIME          PIC 9(06).
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION  USING PARA-BUMONCD
                                             PARA-TANCD
                                             PARA-UPDTDATE
                                             PARA-UPDTIME.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      DSPF.
     MOVE     "DSPF    "     TO   ERR-FL-ID.
     MOVE     DSP-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZSOKMS.
     MOVE     "ZSOKMS"       TO   ERR-FL-ID.
     MOVE     SOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    MSTLOGF.
     MOVE     "MASTLOGF"     TO   ERR-FL-ID.
     MOVE     MSL-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
*# 2020/05/14 NAV ST
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "JYOKEN1 "     TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*# 2020/05/14 NAV DT
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
*      ■０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             DSPF.
     OPEN    I-O             ZSOKMS   MSTLOGF.
*# 2020/05/14 NAV ST
     OPEN    INPUT           HJYOKEN.
*# 2020/05/14 NAV ED
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
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*受渡しパラメタセット
     MOVE      DATE-AREA          TO   PARA-UPDTDATE.
     MOVE      WK-TIME(1:6)       TO   PARA-UPDTIME.
*
     MOVE    "FIT01501"      TO   DSP-FORMAT.
     MOVE    SPACE           TO   DSP-FIT01501.
     MOVE    SPACE           TO   END-FLG.
     MOVE    "1"             TO   MAIN-FLG.
     MOVE    2               TO   WK-SYORI.
     MOVE    SPACE           TO   DSP-PROC.
 INIT-END.
     EXIT.
****************************************************************
*      ■０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
     EVALUATE      MAIN-FLG
         WHEN      "1"       PERFORM   SYORI-SUB
         WHEN      "2"       PERFORM   HEAD-SUB
         WHEN      "3"       PERFORM   BODY-SUB
         WHEN      "4"       PERFORM   KAKUNIN-SUB
         WHEN      "5"       PERFORM   FILPRT-SUB
         WHEN      OTHER     CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1       処理区分入力                                  *
*--------------------------------------------------------------*
 SYORI-SUB                   SECTION.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SOKCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SOKCD.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-KAKNIN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-KAKNIN.
     PERFORM       MSG-SEC.
     MOVE     PMSG03    TO   DSP-MSG2.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "SYORI"    TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE    DSP-FUNC
       WHEN
        "F005"
           MOVE   "END"        TO   END-FLG
       WHEN
        "E000"
           PERFORM        SYORICHK-SUB
                  IF     ERR-MSG-CD   =    ZERO
                         MOVE   "2"   TO   MAIN-FLG
                         MOVE   "D"     TO
                                EDIT-OPTION  OF  DSP-SYORI
                         MOVE   SPACE   TO
                                EDIT-CURSOR  OF  DSP-SYORI
                  END-IF
       WHEN
         OTHER
           MOVE   01     TO   ERR-MSG-CD
     END-EVALUATE.
*
 SYORI-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.1     画面表示処理                                  *
*--------------------------------------------------------------*
 DSP-WRITE-SUB               SECTION.
     MOVE    "GPALL"         TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     HEN-DATE       TO   DSP-SDATE.
     MOVE     HEN-TIME       TO   DSP-STIME.
     WRITE    DSP-FIT01501.
 DSP-WRITE-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.2     エラーメッセージセット                        *
*--------------------------------------------------------------*
 MSG-SEC                     SECTION.
* エラー メッセージ セット
     IF  ERR-MSG-CD     =    ZERO
         MOVE    SPACE       TO   DSP-MSG1
     ELSE
         MOVE    ERR-MSG(ERR-MSG-CD)   TO   DSP-MSG1
         MOVE    ZERO                  TO   ERR-MSG-CD
     END-IF.
 MSG-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.3     画面データの入力処理                          *
*--------------------------------------------------------------*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"         TO   DSP-PROC.
     READ   DSPF.
 DSP-READ-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.4     処理区分の入力チェック                        *
*--------------------------------------------------------------*
 SYORICHK-SUB            SECTION.
*処理区分 CHK
     IF  ( DSP-SYORI  NOT  NUMERIC   )
         MOVE   05      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SYORI
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SYORI
     ELSE
         IF  ( DSP-SYORI  =  1 OR 2 OR 3 )
             IF  DSP-SYORI      =    1
                 MOVE    1           TO   WK-SYORI
             END-IF
             IF  DSP-SYORI      =    2
                 MOVE    2           TO   WK-SYORI
             END-IF
             IF  DSP-SYORI      =    3
                 MOVE    3           TO   WK-SYORI
             END-IF
         ELSE
             MOVE   05      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SYORI
             MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SYORI
         END-IF
     END-IF.
*
 SYORICHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2       ＨＥＡＤ部入力                                *
*--------------------------------------------------------------*
 HEAD-SUB                    SECTION.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SOKNM1.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SOKNM1.
***  97.03.11 ST  ***
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-UNTEN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-UNTEN.
***  97.03.11 ED  ***
***  11.04.14 ST  ***
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SYUSND.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SYUSND.
***  11.04.14 ED  ***
***  11.05.31 ST  ***
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TEGSND.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-TEGSND.
***  11.05.31 ED  ***
*****2012/07/24 ADD    ST 小売連携区分　属性初期化
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-KOURI.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-KOURI.
*****2012/07/24 ADD    ED 小売連携区分　属性初期化
*****2012/10/09 ADD    ST 物流連携区分　属性初期化
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-BUTUR.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-BUTUR.
*****2012/10/09 ADD    ED 物流連携区分　属性初期化
*****2020/02/12 ADD    ST 倉庫区分　属性初期化
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SOKKBN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SOKKBN.
*****2020/02/12 ADD    ED 倉庫区分　属性初期化
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-KAKNIN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-KAKNIN.
     PERFORM       MSG-SEC.
     MOVE     PMSG01    TO   DSP-MSG2.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "GPHEAD"   TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE    DSP-FUNC
       WHEN
        "F004"
           PERFORM        HEADDEL-SUB
           PERFORM        BODYDEL-SUB
           MOVE   "1"          TO   MAIN-FLG
           MOVE   ZERO         TO   ERR-MSG-CD
       WHEN
        "E000"
           PERFORM        HEADCHK-SUB
                  IF     ERR-MSG-CD   =    ZERO
                         IF  WK-SYORI  =   3
                             MOVE   "4"   TO   MAIN-FLG
                         ELSE
                             MOVE   "3"   TO   MAIN-FLG
                         END-IF
                         MOVE   "D"     TO
                                EDIT-OPTION  OF  DSP-SOKCD
                         MOVE   SPACE   TO
                                EDIT-CURSOR  OF  DSP-SOKCD
                  END-IF
       WHEN
         OTHER
           MOVE   01     TO   ERR-MSG-CD
     END-EVALUATE.
*
 HEAD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.1     ＨＥＡＤ部消去                                *
*--------------------------------------------------------------*
 HEADDEL-SUB            SECTION.
     MOVE  SPACE        TO   DSP-SOKCD.
 HEADDEL-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2     ＨＥＡＤ部の入力チェック                      *
*--------------------------------------------------------------*
 HEADCHK-SUB            SECTION.
*
     IF  DSP-SOKCD      =    SPACE
         MOVE   04      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SOKCD
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SOKCD
     ELSE
         PERFORM   FILCHK-SUB
     END-IF.
*
 HEADCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2.1   ファイルの存在チェック                        *
*--------------------------------------------------------------*
 FILCHK-SUB             SECTION.
     MOVE    ZERO            TO   ERR-MSG-CD.
     MOVE    DSP-SOKCD       TO   SOK-F01.
     READ    ZSOKMS
         INVALID   KEY
             MOVE  1         TO   INVALID-FLG
         NOT INVALID KEY
             MOVE  ZERO      TO   INVALID-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF  INVALID-FLG    =    1
         IF   WK-SYORI   NOT =   1
              MOVE   03      TO   ERR-MSG-CD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SOKCD
              MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SOKCD
         END-IF
     ELSE
* ＮＯＴ ＩＮＶＡＬＩＤ処理
         IF   WK-SYORI       =    1
              MOVE   02      TO   ERR-MSG-CD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SOKCD
              MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SOKCD
         ELSE
              PERFORM        FILE-SUB
         END-IF
     END-IF.
*
 FILCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2.2    ファイルセット                               *
*--------------------------------------------------------------*
 FILE-SUB               SECTION.
*↓2019.03.22
     MOVE    SOK-REC          TO   BEFORE-DATA.
*↑2019.03.22
     MOVE    SOK-F02          TO   DSP-SOKNM1.
     MOVE    SOK-F03          TO   DSP-SOKNM2.
*----------- 99/11/03 追加 -------------------*
     MOVE    SOK-F111         TO   DSP-NYBN1.
     MOVE    SOK-F112         TO   DSP-NYBN2.
*---------------------------------------------*
     MOVE    SOK-F04          TO   DSP-YUBIN.
     MOVE    SOK-F05          TO   DSP-JUSYO1.
     MOVE    SOK-F06          TO   DSP-JUSYO2.
     MOVE    SOK-F07          TO   DSP-TELNO.
     MOVE    SOK-F08          TO   DSP-FAXNO.
     MOVE    SOK-F09          TO   DSP-KENCD.
***  97.03.10 ST  ***
     MOVE    SOK-F10          TO   DSP-UNTEN.
***  97.03.10 ED  ***
***  11.04.14 ST  ***
*****2012/07/24 NAV UPDATE ST 項目名新規作成の為
*****MOVE    SOK-FIL1(19:1)   TO   DSP-SYUSND.
     MOVE    SOK-F99          TO   DSP-SYUSND.
*****2012/07/24 NAV UPDATE ED 項目名新規作成の為
***  11.04.14 ED  ***
***  11.05.31 ST  ***
*****2012/07/24 NAV UPDATE ST 項目名新規作成の為
*****MOVE    SOK-FIL1(18:1)   TO   DSP-TEGSND.
     MOVE    SOK-F98          TO   DSP-TEGSND.
*****2012/07/24 NAV UPDATE ED 項目名新規作成の為
*****2012/07/24 NAV ADD    ST 小売連携区分追加
     MOVE    SOK-F12          TO   DSP-KOURI.
*****2012/07/24 NAV ADD    ED 小売連携区分追加
*****2012/10/09 NAV ADD    ST 物流連携区分追加
     MOVE    SOK-F13          TO   DSP-BUTUR.
*****2012/10/09 NAV ADD    ED 物流連携区分追加
*****2020/02/12 NAV ADD    ST 倉庫区分追加
     MOVE    SOK-F14          TO   DSP-SOKKBN.
*****2020/02/12 NAV ADD    ED 倉庫区分追加
*****2020/05/14 NAV ADD    ST
     MOVE    SOK-F15          TO   DSP-D365ST.
     MOVE    SOK-F16          TO   DSP-D365SK.
     MOVE    SOK-F17          TO   DSP-D365BS.
*****2020/05/14 NAV ADD    ED
***  11.05.31 ED  ***
     IF   WK-SYORI  =   2
          MOVE  "2"          TO   MAIN-FLG
     ELSE
          MOVE  "3"          TO   MAIN-FLG
     END-IF.
*
 FILE-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3        ＢＯＤＹ部入力                               *
*--------------------------------------------------------------*
 BODY-SUB          SECTION.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-KAKNIN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-KAKNIN.
     PERFORM       MSG-SEC.
     MOVE     PMSG02    TO   DSP-MSG2.
     PERFORM       DSP-WRITE-SUB.
     MOVE   "GPBODY"         TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE      DSP-FUNC
        WHEN  "F004"
                  PERFORM        HEADDEL-SUB
                  PERFORM        BODYDEL-SUB
                  MOVE   "2"          TO   MAIN-FLG
                  MOVE   ZERO         TO   ERR-MSG-CD
        WHEN  "F009"
                  MOVE   "2"          TO   MAIN-FLG
                  MOVE   ZERO         TO   ERR-MSG-CD
        WHEN  "E000"
                  PERFORM        BODYCHK-SUB
                  IF   ERR-MSG-CD      =   ZERO
                       MOVE   "4"     TO   MAIN-FLG
                       MOVE   "D"     TO
                              EDIT-OPTION  OF  DSP-SOKNM1
                       MOVE   SPACE   TO
                              EDIT-CURSOR  OF  DSP-SOKNM1
***                    95.03.11 ST  ***
                       MOVE   "D"     TO
                              EDIT-OPTION  OF  DSP-UNTEN
                       MOVE   SPACE   TO
                              EDIT-CURSOR  OF  DSP-UNTEN
***                    95.03.11 ED  ***
***                    11.04.14 ST  ***
                       MOVE   "D"     TO
                              EDIT-OPTION  OF  DSP-SYUSND
                       MOVE   SPACE   TO
                              EDIT-CURSOR  OF  DSP-SYUSND
***                    11.04.14 ED  ***
***                    11.05.31 ST  ***
                       MOVE   "D"     TO
                              EDIT-OPTION  OF  DSP-TEGSND
                       MOVE   SPACE   TO
                              EDIT-CURSOR  OF  DSP-TEGSND
***                    11.05.31 ED  ***
                  END-IF
        WHEN  OTHER
                  MOVE    01          TO   ERR-MSG-CD
     END-EVALUATE.
*
 BODY-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3.1     ＢＯＤＹ部消去                                *
*--------------------------------------------------------------*
 BODYDEL-SUB            SECTION.
     MOVE  SPACE        TO   DSP-BODY.
 BODYDEL-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3.2     ＢＯＤＹ入力チェック                          *
*--------------------------------------------------------------*
 BODYCHK-SUB            SECTION.
     IF  DSP-SOKNM1     =  SPACE
         MOVE   07      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SOKNM1
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SOKNM1
     ELSE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SOKNM1
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SOKNM1
     END-IF.
***  97.03.11 ST  ***
*    運転手段　チェック
     IF  DSP-UNTEN      =  "01"  OR  "02"  OR  "03"  OR  "99"
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-UNTEN
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-UNTEN
     ELSE
         IF  ERR-MSG-CD  =  ZERO
             MOVE   08      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-UNTEN
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-UNTEN
     END-IF.
***  97.03.11 ED  ***
***  11.04.14 ST  ***
*    出荷送信（オンライン）
     IF  DSP-SYUSND     =  SPACE OR  "1"
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SYUSND
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SYUSND
     ELSE
         IF  ERR-MSG-CD  =  ZERO
             MOVE   09      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SYUSND
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SYUSND
     END-IF.
***  11.04.14 ED  ***
***  11.05.31 ST  ***
*    出荷送信（手書）
     IF  DSP-TEGSND     =  SPACE OR  "1"
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TEGSND
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-TEGSND
     ELSE
         IF  ERR-MSG-CD  =  ZERO
             MOVE   09      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TEGSND
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-TEGSND
     END-IF.
***  11.04.14 ED  ***
*****2012/07/24 ADD    ST 小売連携区分追加
*    出荷送信（手書）
     IF  DSP-KOURI      =  SPACE OR  "1"
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-KOURI
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-KOURI
     ELSE
         IF  ERR-MSG-CD  =  ZERO
            MOVE   10      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-KOURI
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-KOURI
     END-IF.
*****2012/07/24 ADD    ED 小売連携区分追加
*****2012/10/09 ADD    ST 物流連携区分追加
*    出荷送信（手書）
     IF  DSP-BUTUR      =  SPACE OR  "1"
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-BUTUR
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-BUTUR
     ELSE
         IF  ERR-MSG-CD  =  ZERO
             MOVE   11      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-BUTUR
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-BUTUR
     END-IF.
*****2012/10/09 ADD    ED 物流連携区分追加
*****2020/02/12 ADD    ST 倉庫区分追加
     IF  DSP-SOKKBN     =  SPACE OR  "1"
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SOKKBN
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SOKKBN
     ELSE
         IF  ERR-MSG-CD  =  ZERO
             MOVE   11      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SOKKBN
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SOKKBN
     END-IF.
*****2020/02/12 ADD    ED 倉庫区分追加
*****2020/05/15 ADD    ST
*****サイト
     MOVE       41           TO   JYO-F01.
     MOVE       DSP-D365ST   TO   JYO-F02.
     PERFORM  HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG  =  SPACE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-D365ST
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-D365ST
     ELSE
         IF  ERR-MSG-CD  =  ZERO
             MOVE   13      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-D365ST
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-D365ST
     END-IF.
*****倉庫ＣＤ
     IF  DSP-D365SK  =  SPACE
         MOVE   "0"     TO   DSP-D365SK(1:1)
         MOVE DSP-SOKCD TO   DSP-D365SK(2:2)
     END-IF.
*****保管場所
     MOVE       42           TO   JYO-F01.
     MOVE       DSP-D365BS   TO   JYO-F02.
     PERFORM  HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG  =  SPACE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-D365BS
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-D365BS
     ELSE
         IF  ERR-MSG-CD  =  ZERO
             MOVE   14      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-D365BS
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-D365BS
     END-IF.
*****2020/02/12 ADD    ED
 BODYCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.4       確認入力                                      *
*--------------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     IF  ERR-MSG-CD  =  ZERO
         MOVE     "Y"       TO   DSP-KAKNIN
     END-IF.
     PERFORM       MSG-SEC.
     IF  WK-SYORI  =  3
         MOVE     PMSG01    TO   DSP-MSG2
     ELSE
         MOVE     PMSG02    TO   DSP-MSG2
     END-IF.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "GPKAKU"        TO    DSP-GROUP.
     PERFORM       DSP-READ-SUB.
     MOVE     ZERO           TO    ERR-MSG-CD.
 KAKUNIN.
* アテンション判定
     EVALUATE  DSP-FUNC
        WHEN  "F004"
              PERFORM        HEADDEL-SUB
              PERFORM        BODYDEL-SUB
              MOVE   "1"          TO   MAIN-FLG
              MOVE   ZERO         TO   ERR-MSG-CD
              MOVE   SPACE        TO   DSP-KAKNIN
        WHEN  "F009"
              IF   WK-SYORI  =    3
                   MOVE   01      TO   ERR-MSG-CD
              ELSE
                   MOVE   "3"     TO   MAIN-FLG
              END-IF
              MOVE   SPACE        TO   DSP-KAKNIN
        WHEN  "E000"
              IF  DSP-KAKNIN  NOT  =  "Y"
                  MOVE   06      TO   ERR-MSG-CD
              ELSE
                  MOVE   "5"     TO   MAIN-FLG
                  MOVE   SPACE   TO   DSP-KAKNIN
              END-IF
        WHEN  OTHER
              MOVE   01      TO   ERR-MSG-CD
     END-EVALUATE.
*
 KAKUNIN-END.
     EXIT.
*--------------------------------------------------------------*
*      2.5        ファイル更新                                 *
*--------------------------------------------------------------*
 FILPRT-SUB             SECTION.
     IF      WK-SYORI   =    1
             MOVE   SPACE         TO   SOK-REC
             INITIALIZE      SOK-REC
     END-IF.
     MOVE    DSP-SOKCD            TO   SOK-F01.
     MOVE    DSP-SOKNM1           TO   SOK-F02.
     MOVE    DSP-SOKNM2           TO   SOK-F03.
*----------- 99/11/03 追加 -------------------------*
     MOVE    DSP-NYBN1            TO   SOK-F111.
     MOVE    DSP-NYBN2            TO   SOK-F112.
*---------------------------------------------------*
     MOVE    DSP-YUBIN            TO   SOK-F04.
     MOVE    DSP-JUSYO1           TO   SOK-F05.
     MOVE    DSP-JUSYO2           TO   SOK-F06.
     MOVE    DSP-TELNO            TO   SOK-F07.
     MOVE    DSP-FAXNO            TO   SOK-F08.
     MOVE    DSP-KENCD            TO   SOK-F09.
***  95.03.11  ST  ***
     MOVE    DSP-UNTEN            TO   SOK-F10.
***  95.03.11  ED  ***
***  11.04.14  ST  ***
*****2012/07/24 UPDATE ST 項目名新規取得の為
*****MOVE    DSP-SYUSND           TO   SOK-FIL1(19:1).
     MOVE    DSP-SYUSND           TO   SOK-F99.
*****2012/07/24 UPDATE ED 項目名新規取得の為
***  11.04.14  ED  ***
***  11.05.31  ST  ***
*****2012/07/24 UPDATE ST 項目名新規取得の為
*****MOVE    DSP-TEGSND           TO   SOK-FIL1(18:1).
     MOVE    DSP-TEGSND           TO   SOK-F98.
*****2012/07/24 UPDATE ED 項目名新規取得の為
*****2012/07/24 ADD    ST 小売連携区分追加
     MOVE    DSP-KOURI            TO   SOK-F12.
*****2012/07/24 ADD    ED 小売連携区分追加
*****2012/10/09 ADD    ST 物流連携区分追加
     MOVE    DSP-BUTUR            TO   SOK-F13.
*****2012/10/09 ADD    ED 物流連携区分追加
*****2020/02/12 ADD    ST 倉庫区分追加
     MOVE    DSP-SOKKBN           TO   SOK-F14.
*****2020/02/12 ADD    ED 倉庫区分追加
*****2020/05/14 ADD    ST
     MOVE    DSP-D365ST           TO   SOK-F15.
     MOVE    DSP-D365SK           TO   SOK-F16.
     MOVE    DSP-D365BS           TO   SOK-F17.
*****2020/05/14 ADD    ED
***  11.05.31  ED  ***
*  処理モードにより追加・更新・削除
     EVALUATE    WK-SYORI
        WHEN  1
                   MOVE       SOK-REC  TO  DATA-TAIHI
                   WRITE      SOK-REC
                   END-WRITE
        WHEN  2
                   MOVE       SOK-REC  TO  DATA-TAIHI
                   REWRITE    SOK-REC
                   END-REWRITE
        WHEN  3
                   MOVE       SOK-REC  TO  DATA-TAIHI
                   DELETE     ZSOKMS
                   END-DELETE
     END-EVALUATE.
*
     PERFORM  MSTLOGF-WRITE-SEC.
*
     PERFORM       HEADDEL-SUB.
     PERFORM       BODYDEL-SUB.
     MOVE   "1"         TO   MAIN-FLG.
 FILPRT-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      ZSOKMS  MSTLOGF.
 END-END.
     EXIT.
****************************************************************
*            マスタ更新履歴ファイル出力            *
****************************************************************
 MSTLOGF-WRITE-SEC           SECTION.
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
     ACCEPT    WK-TIME          FROM   TIME.
*
     MOVE     "07"                TO   MSL-F01.
     MOVE     PARA-BUMONCD        TO   MSL-F02.
     MOVE     PARA-TANCD          TO   MSL-F03.
     MOVE     WK-SYORI            TO   MSL-F04.
     MOVE     DATE-AREA           TO   MSL-F05.
     MOVE     WK-TIME(1:6)        TO   MSL-F06.
     START    MSTLOGF            KEY IS >= MSL-F01
                                           MSL-F02
                                           MSL-F03
                                           MSL-F04
                                           MSL-F05
                                           MSL-F06
              INVALID
              MOVE     "07"          TO    MSL-F01
              MOVE     PARA-BUMONCD  TO    MSL-F02
              MOVE     PARA-TANCD    TO    MSL-F03
              MOVE     WK-SYORI      TO    MSL-F04
              MOVE     DATE-AREA     TO    MSL-F05
              MOVE     WK-TIME(1:6)  TO    MSL-F06
              MOVE     0             TO    MSL-F07
              MOVE     DATA-TAIHI    TO    MSL-F08
*↓2019.03.22
              PERFORM  BEFORE-SEC
*↑2019.03.22
              WRITE    MSL-REC
              GO TO    MSTLOGF-WRITE-EXIT
     END-START.
     MOVE     0                   TO   SEQ.
*
 MSTLOG-WRITE-010.
     READ   MSTLOGF  NEXT  AT        END
            GO             TO        MSTLOGF-WRITE-EXIT
     END-READ.
*
     IF  (MSL-F01 > "07")       OR (MSL-F02 > PARA-BUMONCD) OR
         (MSL-F03 > PARA-TANCD) OR (MSL-F04 > WK-SYORI)     OR
         (MSL-F05 > DATE-AREA)  OR (MSL-F06 > WK-TIME(1:6))
         MOVE     "07"             TO  MSL-F01
         MOVE     PARA-BUMONCD     TO  MSL-F02
         MOVE     PARA-TANCD       TO  MSL-F03
         MOVE     WK-SYORI         TO  MSL-F04
         MOVE     DATE-AREA        TO  MSL-F05
         MOVE     WK-TIME(1:6)     TO  MSL-F06
         MOVE     SEQ              TO  MSL-F07
         MOVE     DATA-TAIHI       TO  MSL-F08
*↓2019.03.20
         PERFORM  BEFORE-SEC
*↑2019.03.20
         WRITE    MSL-REC
         GO       TO               MSTLOGF-WRITE-EXIT
     ELSE
         COMPUTE  SEQ =  MSL-F07  +  1
         GO       TO               MSTLOG-WRITE-010
     END-IF.
 MSTLOGF-WRITE-EXIT.
     EXIT.
****************************************************************
*  2019.03.22    修正前レコードセット
****************************************************************
 BEFORE-SEC                  SECTION.
*
     IF       DSP-SYORI   =    2
              MOVE  BEFORE-DATA   TO   MSL-F09
     END-IF.
*
 BEFORE-EXIT.
     EXIT.
****************************************************************
*    条件Ｆ読込
****************************************************************
 HJYOKEN-READ-SEC            SECTION.
*
     READ  HJYOKEN
           INVALID      MOVE  "INV"    TO   HJYOKEN-INV-FLG
           NOT  INVALID MOVE  SPACE    TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
