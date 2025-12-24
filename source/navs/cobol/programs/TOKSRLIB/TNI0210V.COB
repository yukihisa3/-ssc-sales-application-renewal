****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　日次　　　　　　　　　　　　　　　*
*    モジュール名　　　　：　日次更新計上エラー　ＣＳＶ出力　　*
*    作成日／作成者　　　：　2022/08/08 INOUE                  *
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　日次更新計上エラーデータのみを　　*
*    　　　　　　　　　　　　ＣＳＶ出力する。　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            TNI0210V.
*                  流用:NVD0810V
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PRIMERGY6000.
 OBJECT-COMPUTER.       PRIMERGY6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YB        IS   YB
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*日次更新計上エラーファイル
     SELECT  DNIERRL1 ASSIGN        TO  DNIERRL1
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  DYNAMIC
                      RECORD KEY    IS  ERR1-F01 *> 日次更新日付
                                        ERR1-F02 *> 日次更新時刻
                                        ERR1-F03 *> 取引先ＣＤ
                                        ERR1-F04 *> 伝票番号
                                        ERR1-F07 *> 相殺区分
                                        ERR1-F08 *> 伝票区分
                                        ERR1-F11 *> 店舗ＣＤ
                                        ERR1-F12 *> 納品日
                                        ERR1-F06 *> 行番号
                      FILE   STATUS IS  ERR1-STATUS.
****<< SUB商品名称マスタ　  >>******************************
     SELECT   SUBMEIL1   ASSIGN  TO   DA-01-VI-SUBMEIL1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  MEI-F011
                                                 MEI-F012
                        FILE STATUS          IS  MEI-STATUS.
****<< 取引先マスタ　　　  >>******************************
     SELECT   TOKMS2   ASSIGN  TO   DA-01-VI-TOKMS2
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  TOK-F01
                        FILE STATUS          IS  TOK-STATUS.
****<< 店舗マスタ　　　  >>******************************
     SELECT   TENMS1   ASSIGN  TO   DA-01-VI-TENMS1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  TEN-F52
                                                 TEN-F011
                        FILE STATUS          IS  TEN-STATUS.
****<< 日次更新計上エラーＣＳＶ >>****************************
     SELECT   DNIERRCV    ASSIGN  TO   DA-01-S-DNIERRCV
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   CSV-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< 日次更新計上エラーファイル       >>********************
 FD    DNIERRL1.
       COPY     DNIERRL1    OF        XFDLIB
       JOINING  ERR1        AS        PREFIX.
****<< SUB商品名称マスタ　　　>>******************************
 FD    SUBMEIL1.
       COPY     SUBMEIL1    OF        XFDLIB
                JOINING   MEI       PREFIX.
****<< 取引先マスタ　　　　　 >>******************************
 FD    TOKMS2.
       COPY     TOKMS2     OF        XFDLIB
                JOINING   TOK       PREFIX.
****<< 店舗マスタ　　　　　 >>******************************
 FD    TENMS1.
       COPY     TENMS1     OF        XFDLIB
                JOINING   TEN       PREFIX.
****<< 日次更新計上エラーＣＳＶ >>************************
 FD    DNIERRCV.
       COPY     DNIERRC4   OF       XFDLIB
                JOINING   CSV       PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ＣＳＶ 帳票タイトル行       ****
 COPY   DNIERRC1   OF        XFDLIB
        JOINING    CSV1      PREFIX.
****  ＣＳＶ ヘッダ行　　　       ****
 COPY   DNIERRC2   OF        XFDLIB
        JOINING    CSV2      PREFIX.
****  ＣＳＶ 項目タイトル行　　   ****
 COPY   DNIERRC3   OF        XFDLIB
        JOINING    CSV3      PREFIX.
****  画面制御項目            ****
*01  DSP-CONTROL.
*    02 DSP-PROC             PIC  X(2).
*    02 DSP-GROUP            PIC  X(8).
*    02 DSP-FORMAT           PIC  X(8).
*    02 DSP-STATUS           PIC  X(2).
*    02 DSP-FUNC             PIC  X(4).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ERR1-STATUS          PIC  X(2).
     02 CSV-STATUS           PIC  X(2).
     02 NYY-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
     02 TOK-STATUS           PIC  X(2).
     02 TEN-STATUS           PIC  X(2).
****  カウンタ                ****
 01  DNIERRF-CNT             PIC  9(8)   VALUE  ZERO.
 01  DNIERRCV-CNT            PIC  9(8)   VALUE  ZERO.
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  INV-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
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
**** エラーメッセージ         ****
*01  ERR-TAB.
*    02  MSG-ERR1            PIC  N(30)  VALUE
*           NC"無効ＰＦキーです。".
*    02  MSG-ERR2            PIC  N(30)  VALUE
*           NC"開始・終了コードの関係に誤りがあります。".
*    02  PMSG01              PIC N(20) VALUE
*           NC"_取消　_終了".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "TNI0210V".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*変換１
 01  HENKAN1                 PIC  9(08)V9(02).
 01  HENKAN1R          REDEFINES      HENKAN1.
     03  HENKAN1-1           PIC  9(08).
     03  HENKAN1-2           PIC  9(02).
*
 01  HENKAN1-DATA.
*    03  HENKAN1-DATA0       PIC  X(01).
     03  HENKAN1-DATA1       PIC  X(08).
     03  HENKAN1-DATA2       PIC  X(01).
     03  HENKAN1-DATA3       PIC  X(02).
*変換２
 01  HENKAN2                 PIC  9(01)V9(02).
 01  HENKAN2R          REDEFINES      HENKAN2.
     03  HENKAN2-1           PIC  9(01).
     03  HENKAN2-2           PIC  9(02).
*
 01  HENKAN2-DATA.
     03  HENKAN2-DATA1       PIC  X(01).
     03  HENKAN2-DATA2       PIC  X(01).
     03  HENKAN2-DATA3       PIC  X(02).
*
 01  WK-KANJI.
     03  WK-KANJI1           PIC  N(15)   VALUE  SPACE.
     03  WK-KANJI2           PIC  N(15)   VALUE  SPACE.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
******************************************************************
 LINKAGE                SECTION.
 01  PARA-IN-NIDATE         PIC   9(08).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION
                           USING PARA-IN-NIDATE.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DNIERRL1.
     MOVE      "DNIERRL1"    TO   ERR-FL-ID.
     MOVE      ERR1-STATUS   TO   ERR-STCD.
     DISPLAY   MSG-ABEND1        UPON CONS.
     DISPLAY   MSG-ABEND2        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DNIERRCV.
     MOVE   "DNIERRCV"        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBMEIL1.
     MOVE   "SUBMEIL1"        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TOKMS2.
     MOVE   "TOKMS2 "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC6           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TENMS1.
     MOVE   "TENMS1 "        TO    ERR-FL-ID.
     MOVE    TEN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 TNI0210V-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 TNI0210V-END.
     EXIT.
************************************************************
*      ■０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
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
*
     OPEN     INPUT     DNIERRL1  SUBMEIL1  TOKMS2  TENMS1.
     OPEN     OUTPUT    DNIERRCV.
*
 010-INIT.
*日次更新計上エラーファイル スタート
     PERFORM  DNIERRL1-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"対象データなし" UPON CONS
          GO                    TO   INIT-END
     END-IF.
*日次更新計上エラーファイル 読込
     PERFORM DNIERRL1-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"対象データなし" UPON CONS
          GO                    TO   INIT-END
     END-IF.
*
 021-INIT.
*    帳票タイトルレコード出力
     MOVE     SPACE                   TO       CSV1-REC.
     MOVE     X"28"                   TO       CSV1-CS01.
     MOVE     NC"＜日次更新エラーリスト＞"
                                      TO       CSV1-C01.
     MOVE     X"29"                   TO       CSV1-CE01.
     MOVE     ","                     TO       CSV1-CK01
     WRITE    CSV-REC                 FROM     CSV1-REC.
*
 022-INIT.
*    ヘッダレコード出力
     MOVE     SPACE                   TO       CSV2-REC.
     MOVE     X"28"                   TO       CSV2-HS01.
     MOVE     NC"日次更新日付："      TO       CSV2-H01.
     MOVE     X"29"                   TO       CSV2-HE01.
     MOVE     ","                     TO       CSV2-HK01.
     MOVE     ERR1-F01(1:4)           TO       CSV2-H02(1:4).
     MOVE     "/"                     TO       CSV2-H02(5:1).
     MOVE     ERR1-F01(5:2)           TO       CSV2-H02(6:2).
     MOVE     "/"                     TO       CSV2-H02(8:1).
     MOVE     ERR1-F01(7:2)           TO       CSV2-H02(9:2).
     MOVE     ","                     TO       CSV2-HK02.
     MOVE     X"28"                   TO       CSV2-HS03.
     MOVE     NC"日次更新時刻："      TO       CSV2-H03.
     MOVE     X"29"                   TO       CSV2-HE03.
     MOVE     ","                     TO       CSV2-HK03.
     MOVE     ERR1-F02(1:2)           TO       CSV2-H04(1:2).
     MOVE     ":"                     TO       CSV2-H04(3:1).
     MOVE     ERR1-F02(3:2)           TO       CSV2-H04(4:2).
     MOVE     ":"                     TO       CSV2-H04(6:1).
     MOVE     ERR1-F02(5:2)           TO       CSV2-H04(7:2).
     MOVE     ","                     TO       CSV2-HK04.
     WRITE    CSV-REC                 FROM     CSV2-REC.
*
 023-INIT.
*    項目タイトルレコード出力
     MOVE     SPACE                   TO       CSV3-REC.
     MOVE     X"28"                   TO       CSV3-KS01
                                               CSV3-KS02
                                               CSV3-KS03
                                               CSV3-KS04
                                               CSV3-KS05
                                               CSV3-KS06
                                               CSV3-KS07
                                               CSV3-KS08
                                               CSV3-KS09
                                               CSV3-KS10
                                               CSV3-KS11
                                               CSV3-KS12
                                               CSV3-KS13
                                               CSV3-KS14
                                               CSV3-KS15
                                               CSV3-KS16
                                               CSV3-KS17
                                               CSV3-KS18
                                               CSV3-KS19
                                               CSV3-KS20
                                               CSV3-KS21
                                               CSV3-KS22
                                               CSV3-KS23
                                               CSV3-KS24
                                               CSV3-KS25
                                               CSV3-KS26
                                               CSV3-KS27
                                               CSV3-KS28
                                               CSV3-KS29
                                               CSV3-KS30
                                               CSV3-KS31
                                               CSV3-KS32
                                               CSV3-KS33
                                               CSV3-KS34
                                               CSV3-KS35
                                               CSV3-KS36
                                               CSV3-KS37
                                               CSV3-KS38
                                               CSV3-KS39
                                               CSV3-KS40
                                               CSV3-KS41
                                               CSV3-KS42
                                               CSV3-KS43
                                               CSV3-KS44
                                               CSV3-KS45
                                               CSV3-KS46
                                               CSV3-KS47
                                               CSV3-KS48
                                               CSV3-KS49.
     MOVE     X"29"                   TO       CSV3-KE01
                                               CSV3-KE02
                                               CSV3-KE03
                                               CSV3-KE04
                                               CSV3-KE05
                                               CSV3-KE06
                                               CSV3-KE07
                                               CSV3-KE08
                                               CSV3-KE09
                                               CSV3-KE10
                                               CSV3-KE11
                                               CSV3-KE12
                                               CSV3-KE13
                                               CSV3-KE14
                                               CSV3-KE15
                                               CSV3-KE16
                                               CSV3-KE17
                                               CSV3-KE18
                                               CSV3-KE19
                                               CSV3-KE20
                                               CSV3-KE21
                                               CSV3-KE22
                                               CSV3-KE23
                                               CSV3-KE24
                                               CSV3-KE25
                                               CSV3-KE26
                                               CSV3-KE27
                                               CSV3-KE28
                                               CSV3-KE29
                                               CSV3-KE30
                                               CSV3-KE31
                                               CSV3-KE32
                                               CSV3-KE33
                                               CSV3-KE34
                                               CSV3-KE35
                                               CSV3-KE36
                                               CSV3-KE37
                                               CSV3-KE38
                                               CSV3-KE39
                                               CSV3-KE40
                                               CSV3-KE41
                                               CSV3-KE42
                                               CSV3-KE43
                                               CSV3-KE44
                                               CSV3-KE45
                                               CSV3-KE46
                                               CSV3-KE47
                                               CSV3-KE48
                                               CSV3-KE49.
     MOVE     ","                     TO       CSV3-KK01
                                               CSV3-KK02
                                               CSV3-KK03
                                               CSV3-KK04
                                               CSV3-KK05
                                               CSV3-KK06
                                               CSV3-KK07
                                               CSV3-KK08
                                               CSV3-KK09
                                               CSV3-KK10
                                               CSV3-KK11
                                               CSV3-KK12
                                               CSV3-KK13
                                               CSV3-KK14
                                               CSV3-KK15
                                               CSV3-KK16
                                               CSV3-KK17
                                               CSV3-KK18
                                               CSV3-KK19
                                               CSV3-KK20
                                               CSV3-KK21
                                               CSV3-KK22
                                               CSV3-KK23
                                               CSV3-KK24
                                               CSV3-KK25
                                               CSV3-KK26
                                               CSV3-KK27
                                               CSV3-KK28
                                               CSV3-KK29
                                               CSV3-KK30
                                               CSV3-KK31
                                               CSV3-KK32
                                               CSV3-KK33
                                               CSV3-KK34
                                               CSV3-KK35
                                               CSV3-KK36
                                               CSV3-KK37
                                               CSV3-KK38
                                               CSV3-KK39
                                               CSV3-KK40
                                               CSV3-KK41
                                               CSV3-KK42
                                               CSV3-KK43
                                               CSV3-KK44
                                               CSV3-KK45
                                               CSV3-KK46
                                               CSV3-KK47
                                               CSV3-KK48
                                               CSV3-KK49.
     MOVE   NC"取引先ＣＤ"             TO      CSV3-K01.
     MOVE   NC"取引先名"               TO      CSV3-K02.
     MOVE   NC"伝票番号"               TO      CSV3-K03.
     MOVE   NC"たねまる伝票番号"       TO      CSV3-K04.
     MOVE   NC"行番号"                 TO      CSV3-K05.
     MOVE   NC"相殺区分"               TO      CSV3-K06.
     MOVE   NC"伝票区分"               TO      CSV3-K07.
     MOVE   NC"担当者ＣＤ"             TO      CSV3-K08.
     MOVE   NC"出荷場所"               TO      CSV3-K09.
     MOVE   NC"店舗ＣＤ"               TO      CSV3-K10.
     MOVE   NC"店舗名"                 TO      CSV3-K11.
     MOVE   NC"納品日"                 TO      CSV3-K12.
     MOVE   NC"出荷日"                 TO      CSV3-K13.
     MOVE   NC"サカタ商品ＣＤ"         TO      CSV3-K14.
     MOVE   NC"サカタ品単ＣＤ"         TO      CSV3-K15.
     MOVE   NC"相手商品ＣＤ"           TO      CSV3-K16.
     MOVE   NC"たねまるＪＡＮＣＤ"     TO      CSV3-K17.
     MOVE   NC"商品名１"               TO      CSV3-K18.
     MOVE   NC"商品名２"               TO      CSV3-K19.
     MOVE   NC"受注時数量"             TO      CSV3-K20.
     MOVE   NC"出荷時数量"             TO      CSV3-K21.
     MOVE   NC"原価単価"               TO      CSV3-K22.
     MOVE   NC"売価単価"               TO      CSV3-K23.
     MOVE   NC"付番区分"               TO      CSV3-K24.
     MOVE   NC"売上計上区分"           TO      CSV3-K25.
     MOVE   NC"ＯＲＤセット区分"       TO      CSV3-K26.
     MOVE   NC"連携番号"               TO      CSV3-K27.
     MOVE   NC"ナフコ管理番号"         TO      CSV3-K28.
     MOVE   NC"受信日"                 TO      CSV3-K29.
     MOVE   NC"受信時刻"               TO      CSV3-K30.
     MOVE   NC"振分倉庫"               TO      CSV3-K31.
     MOVE   NC"需要家ＩＤ"             TO      CSV3-K32.
     MOVE   NC"在庫調整フラグ"         TO      CSV3-K33.
     MOVE   NC"先行受注時数量"         TO      CSV3-K34.
     MOVE   NC"商品変換ＴＢＬ未登録"   TO      CSV3-K35.
     MOVE   NC"消費変換ＴＢＬ不一致"   TO      CSV3-K36.
     MOVE   NC"商品未登録"             TO      CSV3-K37.
     MOVE   NC"店舗未登録"             TO      CSV3-K38.
     MOVE   NC"ＳＵＢ商品未登録"       TO      CSV3-K39.
     MOVE   NC"取引先未登録"           TO      CSV3-K40.
     MOVE   NC"先行分数量エラー"       TO      CSV3-K41.
     MOVE   NC"税区分エラー"           TO      CSV3-K42.
     MOVE   NC"在庫調整戻りエラー"     TO      CSV3-K43.
     MOVE   NC"顧客需要家エラー"       TO      CSV3-K44.
     MOVE   NC"エラー区分１１"         TO      CSV3-K45.
     MOVE   NC"エラー区分１２"         TO      CSV3-K46.
     MOVE   NC"エラー区分１３"         TO      CSV3-K47.
     MOVE   NC"エラー区分１４"         TO      CSV3-K48.
     MOVE   NC"エラー区分１５"         TO      CSV3-K49.
     WRITE  CSV-REC                    FROM    CSV3-REC.
*
 INIT-END.
     EXIT.
************************************************************
*      ■０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
*
     ADD        1           TO      DNIERRF-CNT.
*
*日次更新計上エラーＣＳＶ出力
 MAIN-01.
*
     PERFORM  DNIERRCV-WRITE1-SEC.
*
 MAIN-02.
     PERFORM  DNIERRL1-READ-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*    日次更新計上エラーファイル スタート
****************************************************************
 DNIERRL1-START-SEC          SECTION.
*
*    MOVE    "DNIERRL1-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   ERR1-REC.
     INITIALIZE                        ERR1-REC.
     MOVE     PARA-IN-NIDATE      TO   ERR1-F01.
*
     START  DNIERRL1  KEY  IS  >=      ERR1-F01
                                       ERR1-F02
                                       ERR1-F03
                                       ERR1-F04
                                       ERR1-F07
                                       ERR1-F08
                                       ERR1-F11
                                       ERR1-F12
                                       ERR1-F06
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 DNIERRL1-START-EXIT.
     EXIT.
*
****************************************************************
*    日次更新計上エラーファイル 読込
****************************************************************
 DNIERRL1-READ-SEC           SECTION.
*
*    MOVE    "DNIERRL1-READ-SEC"  TO   S-NAME.
*
 DNIERRL1-READ-010.
     READ     DNIERRL1       NEXT
         AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   DNIERRL1-READ-EXIT
     END-READ.
*日次更新日を判定
     IF       ERR1-F01   NOT =  PARA-IN-NIDATE
              MOVE     "END"      TO   END-FLG
              GO                  TO   DNIERRL1-READ-EXIT
     END-IF.
*エラー区分チェック
     IF     ( ERR1-F35   NOT =  "1" ) AND
            ( ERR1-F36   NOT =  "1" ) AND
            ( ERR1-F37   NOT =  "1" ) AND
            ( ERR1-F38   NOT =  "1" ) AND
            ( ERR1-F39   NOT =  "1" ) AND
            ( ERR1-F40   NOT =  "1" ) AND
            ( ERR1-F41   NOT =  "1" ) AND
            ( ERR1-F42   NOT =  "1" ) AND
            ( ERR1-F43   NOT =  "1" ) AND
            ( ERR1-F44   NOT =  "1" ) AND
            ( ERR1-F45   NOT =  "1" ) AND
            ( ERR1-F46   NOT =  "1" ) AND
            ( ERR1-F47   NOT =  "1" ) AND
            ( ERR1-F48   NOT =  "1" ) AND
            ( ERR1-F49   NOT =  "1" )
              GO                  TO   DNIERRL1-READ-010
     END-IF.
*件数カウント
     ADD      1                   TO   DNIERRF-CNT.
*
 DNIERRL1-READ-EXIT.
     EXIT.
*
************************************************************
*      １   日次更新計上エラーＣＳＶ出力　L1より　　　     *
************************************************************
 DNIERRCV-WRITE1-SEC       SECTION.
*
*レコード初期化　　　　　　　　　　
     MOVE     SPACE         TO      CSV-REC.
     INITIALIZE                     CSV-REC.
*カンマセット
     MOVE     ","           TO      CSV-MK01 CSV-MK02 CSV-MK03
                                    CSV-MK04 CSV-MK05 CSV-MK06
                                    CSV-MK07 CSV-MK08 CSV-MK09
                                    CSV-MK10 CSV-MK11 CSV-MK12
                                    CSV-MK13 CSV-MK14 CSV-MK15
                                    CSV-MK16 CSV-MK17 CSV-MK18
                                    CSV-MK19 CSV-MK20 CSV-MK21
                                    CSV-MK22 CSV-MK23 CSV-MK24
                                    CSV-MK25 CSV-MK26 CSV-MK27
                                    CSV-MK28 CSV-MK29 CSV-MK30
                                    CSV-MK31 CSV-MK32 CSV-MK33
                                    CSV-MK34 CSV-MK35 CSV-MK36
                                    CSV-MK37 CSV-MK38 CSV-MK39
                                    CSV-MK40 CSV-MK41 CSV-MK42
                                    CSV-MK43 CSV-MK44 CSV-MK45
                                    CSV-MK46 CSV-MK47 CSV-MK48
                                    CSV-MK49.
     MOVE     X"28"         TO      CSV-MS02 CSV-MS11
                                    CSV-MS18 CSV-MS19.
     MOVE     X"29"         TO      CSV-ME02 CSV-ME11
                                    CSV-ME18 CSV-ME19.
*カウントアップ
     ADD       1            TO      DNIERRCV-CNT.
*
*取引先ＣＤ
     MOVE      ERR1-F03      TO      CSV-M01.
*取引先名
*　取引先マスタ検索
     MOVE      SPACE         TO      TOK-REC.
     INITIALIZE                      TOK-REC.
     MOVE      ERR1-F03      TO      TOK-F01.
     READ      TOKMS2
       INVALID
         MOVE  ALL NC"＊"    TO      CSV-M02
       NOT INVALID
         MOVE  TOK-F02       TO      CSV-M02
     END-READ.
*伝票番号
     MOVE      ERR1-F04      TO      CSV-M03.
*たねまる伝票番号
     MOVE      ERR1-F05      TO      CSV-M04.
*行_
     MOVE      ERR1-F06      TO      CSV-M05.
*相殺区分
     MOVE      ERR1-F07      TO      CSV-M06.
*伝票区分
     MOVE      ERR1-F08      TO      CSV-M07.
*担当者ＣＤ
     MOVE      ERR1-F09      TO      CSV-M08.
*出荷場所
     MOVE      ERR1-F10      TO      CSV-M09.
*店舗ＣＤ
     MOVE      ERR1-F11      TO      CSV-M10.
*店舗名
*　店舗マスタ検索
     MOVE      SPACE         TO      TEN-REC.
     INITIALIZE                      TEN-REC.
     MOVE      ERR1-F03      TO      TEN-F52.
     MOVE      ERR1-F11      TO      TEN-F011.
     READ      TENMS1
       INVALID
         MOVE  ALL NC"＊"    TO      CSV-M11
       NOT INVALID
         MOVE  TEN-F03       TO      CSV-M11
     END-READ.
*納品日
     MOVE      ERR1-F12(1:4) TO      CSV-M12(1:4).
     MOVE      "/"           TO      CSV-M12(5:1).
     MOVE      ERR1-F12(5:2) TO      CSV-M12(6:2).
     MOVE      "/"           TO      CSV-M12(8:1).
     MOVE      ERR1-F12(7:2) TO      CSV-M12(9:2).
*出荷日
     MOVE      ERR1-F13(1:4) TO      CSV-M13(1:4).
     MOVE      "/"           TO      CSV-M13(5:1).
     MOVE      ERR1-F13(5:2) TO      CSV-M13(6:2).
     MOVE      "/"           TO      CSV-M13(8:1).
     MOVE      ERR1-F13(7:2) TO      CSV-M13(9:2).
*サカタ商品ＣＤ
     MOVE      ERR1-F14      TO      CSV-M14.
*サカタ品単ＣＤ
     MOVE      ERR1-F15      TO      CSV-M15.
*相手商品ＣＤ
     MOVE      ERR1-F16      TO      CSV-M16.
*たねまるＪＡＮＣＤ
     MOVE      ERR1-F17      TO      CSV-M17.
*商品名１,２
*　商品名称マスタ検索
     MOVE      SPACE         TO      MEI-REC.
     INITIALIZE                      MEI-REC.
     MOVE      ERR1-F14      TO      MEI-F011.
     MOVE      ERR1-F15      TO      MEI-F012.
     READ      SUBMEIL1
       INVALID
         MOVE  ALL NC"＊"    TO      CSV-M18
                                     CSV-M19
       NOT INVALID
         MOVE  MEI-F021      TO      CSV-M18
         MOVE  MEI-F022      TO      CSV-M19
     END-READ.
*受注時数量
     IF        ERR1-F20      <       0
               MOVE   "-"    TO      CSV-MA20
     ELSE
               MOVE   "0"    TO      CSV-MA20
     END-IF.
     MOVE      ERR1-F20      TO      HENKAN1.
     MOVE      HENKAN1-1     TO      HENKAN1-DATA1.
     MOVE      "."           TO      HENKAN1-DATA2.
     MOVE      HENKAN1-2     TO      HENKAN1-DATA3.
     MOVE      HENKAN1-DATA  TO      CSV-M20.
*出荷時数量
     IF        ERR1-F21      <       0
               MOVE   "-"    TO      CSV-MA21
     ELSE
               MOVE   "0"    TO      CSV-MA21
     END-IF.
     MOVE      ERR1-F21      TO      HENKAN1.
     MOVE      HENKAN1-1     TO      HENKAN1-DATA1.
     MOVE      "."           TO      HENKAN1-DATA2.
     MOVE      HENKAN1-2     TO      HENKAN1-DATA3.
     MOVE      HENKAN1-DATA  TO      CSV-M21.
*原価単価
     IF        ERR1-F22      <       0
               MOVE   "-"    TO      CSV-MA22
     ELSE
               MOVE   "0"    TO      CSV-MA22
     END-IF.
     MOVE      ERR1-F22      TO      HENKAN1.
     MOVE      HENKAN1-1     TO      HENKAN1-DATA1.
     MOVE      "."           TO      HENKAN1-DATA2.
     MOVE      HENKAN1-2     TO      HENKAN1-DATA3.
     MOVE      HENKAN1-DATA  TO      CSV-M22.
*売価単価
     IF        ERR1-F23      <       0
               MOVE   "-"    TO      CSV-MA23
     ELSE
               MOVE   "0"    TO      CSV-MA23
     END-IF.
     MOVE      ERR1-F23      TO      HENKAN1.
     MOVE      HENKAN1-1     TO      HENKAN1-DATA1.
     MOVE      "."           TO      HENKAN1-DATA2.
     MOVE      HENKAN1-2     TO      HENKAN1-DATA3.
     MOVE      HENKAN1-DATA  TO      CSV-M23.
*付番区分
     MOVE      ERR1-F24      TO      CSV-M24.
*売上計上区分
     MOVE      ERR1-F25      TO      CSV-M25.
*ＯＲＤセット区分
     MOVE      ERR1-F26      TO      CSV-M26.
*連携番号
     MOVE      ERR1-F27      TO      CSV-M27.
*ナフコ管理番号
     MOVE      ERR1-F28      TO      CSV-M28.
*受信日
     MOVE      ERR1-F29(1:4) TO      CSV-M29(1:4).
     MOVE      "/"           TO      CSV-M29(5:1).
     MOVE      ERR1-F29(5:2) TO      CSV-M29(6:2).
     MOVE      "/"           TO      CSV-M29(8:1).
     MOVE      ERR1-F29(7:2) TO      CSV-M29(9:2).
*受信時刻
     MOVE      ERR1-F30(1:2) TO      CSV-M30(1:2).
     MOVE      ":"           TO      CSV-M30(3:1).
     MOVE      ERR1-F30(3:2) TO      CSV-M30(4:2).
*振分倉庫
     MOVE      ERR1-F31      TO      CSV-M31.
*需要家ID
     MOVE      ERR1-F32      TO      CSV-M32.
*在庫調整フラグ
     MOVE      ERR1-F33      TO      CSV-M33.
*先行受注時数量
     IF        ERR1-F34      <       0
               MOVE   "-"    TO      CSV-MA34
     ELSE
               MOVE   "0"    TO      CSV-MA34
     END-IF.
     MOVE      ERR1-F34      TO      HENKAN1.
     MOVE      HENKAN1-1     TO      HENKAN1-DATA1.
     MOVE      "."           TO      HENKAN1-DATA2.
     MOVE      HENKAN1-2     TO      HENKAN1-DATA3.
     MOVE      HENKAN1-DATA  TO      CSV-M34.
*商品変換TBL未登録
     MOVE      ERR1-F35      TO      CSV-M35.
*消費変換TBL不一致
     MOVE      ERR1-F36      TO      CSV-M36.
*商品未登録
     MOVE      ERR1-F37      TO      CSV-M37.
*店舗未登録
     MOVE      ERR1-F38      TO      CSV-M38.
*SUB商品未登録
     MOVE      ERR1-F39      TO      CSV-M39.
*取引先未登録
     MOVE      ERR1-F40      TO      CSV-M40.
*先行分数量エラー
     MOVE      ERR1-F41      TO      CSV-M41.
*税区分エラー
     MOVE      ERR1-F42      TO      CSV-M42.
*在庫調整戻りエラー
     MOVE      ERR1-F43      TO      CSV-M43.
*顧客需要家エラー
     MOVE      ERR1-F44      TO      CSV-M44.
*エラー区分１１
     MOVE      ERR1-F45      TO      CSV-M45.
*エラー区分１２
     MOVE      ERR1-F46      TO      CSV-M46.
*エラー区分１３
     MOVE      ERR1-F47      TO      CSV-M47.
*エラー区分１４
     MOVE      ERR1-F48      TO      CSV-M48.
*エラー区分１５
     MOVE      ERR1-F49      TO      CSV-M49.
*
*レコード出力
     WRITE     CSV-REC.

 DNIERRCV-WRITE1-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE   DNIERRCV DNIERRL1 TOKMS2 TENMS1 SUBMEIL1.
*
     IF      DNIERRF-CNT NOT = ZERO
             DISPLAY  " DNIERRF-READ-CNT   = " DNIERRF-CNT
                                                    UPON CONS
             DISPLAY  " DNIERRCV-WRITE-CNT = " DNIERRCV-CNT
                                                    UPON CONS
     ELSE
             DISPLAY  " DNIERRF-READ-CNT   = " DNIERRF-CNT
                                                    UPON CONS
             DISPLAY  " DNIERRCV-WRITE-CNT = " DNIERRCV-CNT
                                                    UPON CONS
             MOVE    4010      TO     PROGRAM-STATUS
     END-IF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
