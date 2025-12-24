****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＣＳＶ出力　　　　　　　　　　　　*
*    モジュール名　　　　：　入荷予定リストＣＳＶ出力　        *
*    作成日／作成者　　　：　2022/07/20 INOUE                  *
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　入荷予定リストＣＳＶ出力を行う。　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVD0152V.
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
****<< 入荷予定リストＷ　   >>******************************
     SELECT   NYYOTEIF  ASSIGN  TO   DA-01-S-NYYOTEIF
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        FILE STATUS          IS  NYY-STATUS.
****<< SUB商品名称マスタ　  >>******************************
     SELECT   SUBMEIL1   ASSIGN  TO   DA-01-VI-SUBMEIL1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE STATUS          IS  MEI-STATUS.
****<< 倉庫マスタ　　　　  >>******************************
     SELECT   ZSOKMS1   ASSIGN  TO   DA-01-VI-ZSOKMS1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  SOK-F01
                        FILE STATUS          IS  SOK-STATUS.
****<< 条件ファイル　　　  >>******************************
     SELECT   JYOKEN1   ASSIGN  TO   DA-01-VI-JYOKEN1
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  JYO-F01
                                                 JYO-F02
                        FILE STATUS          IS  JYO-STATUS.
****<< 入荷予定リストＣＳＶ >>******************************
     SELECT   NYYOTECV    ASSIGN  TO   DA-01-S-NYYOTECV
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   CSV-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< 入荷予定リストＷ　　　 >>******************************
 FD    NYYOTEIF
       LABEL    RECORD      IS      STANDARD
       BLOCK    CONTAINS    6       RECORDS.
       COPY     NYYOTEIF    OF        XFDLIB
                JOINING   NYY       PREFIX.
****<< SUB商品名称マスタ　　　>>******************************
 FD    SUBMEIL1.
       COPY     SUBMEIL1    OF        XFDLIB
                JOINING   MEI       PREFIX.
****<< 倉庫マスタ　　　　　　 >>******************************
 FD    ZSOKMS1.
       COPY     ZSOKMS1     OF        XFDLIB
                JOINING   SOK       PREFIX.
****<< 条件ファイル　　　　　 >>******************************
 FD    JYOKEN1.
       COPY     JYOKEN1     OF        XFDLIB
                JOINING   JYO       PREFIX.
****<< 入荷予定リストＣＳＶ >>******************************
 FD    NYYOTECV.
       COPY     NYYOTEC2   OF       XFDLIB
                JOINING   CSV       PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ＣＳＶ タイトル行       ****
 COPY   NYYOTEC1   OF        XFDLIB
        JOINING    CSV1      PREFIX.
****  画面制御項目            ****
*01  DSP-CONTROL.
*    02 DSP-PROC             PIC  X(2).
*    02 DSP-GROUP            PIC  X(8).
*    02 DSP-FORMAT           PIC  X(8).
*    02 DSP-STATUS           PIC  X(2).
*    02 DSP-FUNC             PIC  X(4).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 CSV-STATUS           PIC  X(2).
     02 NYY-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
     02 SOK-STATUS           PIC  X(2).
     02 JYO-STATUS           PIC  X(2).
****  カウンタ                ****
 01  NYYOTEIF-CNT            PIC  9(8)   VALUE  ZERO.
 01  NYYOTECV-CNT            PIC  9(8)   VALUE  ZERO.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NVD0152V".
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
     03  HENKAN1-DATA0       PIC  X(01).
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
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYYOTECV.
     MOVE   "NYYOTECV"        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
*FILEERR-SEC2           SECTION.
*    USE AFTER     EXCEPTION
*                  PROCEDURE  DSPF.
*    MOVE   "DSPF    "        TO    ERR-FL-ID.
*    MOVE    DSP-STATUS       TO    ERR-STCD.
*    DISPLAY MSG-ABEND1       UPON  CONS.
*    DISPLAY MSG-ABEND2       UPON  CONS.
*    MOVE    4000             TO    PROGRAM-STATUS.
*    STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYYOTEIF.
     MOVE   "NYYOTEIF"        TO    ERR-FL-ID.
     MOVE    NYY-STATUS       TO    ERR-STCD.
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
                   PROCEDURE  ZSOKMS1.
     MOVE   "ZSOKMS1 "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC6           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  JYOKEN1.
     MOVE   "JYOKEN1 "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 NVD0152V-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 NVD0152V-END.
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
     OPEN     INPUT     NYYOTEIF  SUBMEIL1  ZSOKMS1  JYOKEN1.
     OPEN     OUTPUT    NYYOTECV.
*
 010-INIT.
     READ     NYYOTEIF
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-READ.
*
 020-INIT.
*    項目タイトルレコード出力
     MOVE     SPACE                   TO     CSV1-REC.
     MOVE     X"28"                   TO     CSV1-KS01
                                             CSV1-KS02
                                             CSV1-KS03
                                             CSV1-KS04
                                             CSV1-KS05
                                             CSV1-KS06
                                             CSV1-KS07
                                             CSV1-KS08
                                             CSV1-KS09
                                             CSV1-KS10
                                             CSV1-KS11
                                             CSV1-KS12
                                             CSV1-KS13
                                             CSV1-KS14
                                             CSV1-KS15
                                             CSV1-KS16
                                             CSV1-KS17
                                             CSV1-KS18
                                             CSV1-KS19.
     MOVE     X"29"                   TO     CSV1-KE01
                                             CSV1-KE02
                                             CSV1-KE03
                                             CSV1-KE04
                                             CSV1-KE05
                                             CSV1-KE06
                                             CSV1-KE07
                                             CSV1-KE08
                                             CSV1-KE09
                                             CSV1-KE10
                                             CSV1-KE11
                                             CSV1-KE12
                                             CSV1-KE13
                                             CSV1-KE14
                                             CSV1-KE15
                                             CSV1-KE16
                                             CSV1-KE17
                                             CSV1-KE18
                                             CSV1-KE19.
     MOVE     ","                     TO     CSV1-KK01
                                             CSV1-KK02
                                             CSV1-KK03
                                             CSV1-KK04
                                             CSV1-KK05
                                             CSV1-KK06
                                             CSV1-KK07
                                             CSV1-KK08
                                             CSV1-KK09
                                             CSV1-KK10
                                             CSV1-KK11
                                             CSV1-KK12
                                             CSV1-KK13
                                             CSV1-KK14
                                             CSV1-KK15
                                             CSV1-KK16
                                             CSV1-KK17
                                             CSV1-KK18
                                             CSV1-KK19.
     MOVE   NC"倉庫コード"           TO      CSV1-K01.
     MOVE   NC"倉庫名"               TO      CSV1-K02.
     MOVE   NC"仕入先コード"         TO      CSV1-K03.
     MOVE   NC"発注_"               TO      CSV1-K04.
     MOVE   NC"行_"                 TO      CSV1-K05.
     MOVE   NC"入荷予定日"           TO      CSV1-K06.
     MOVE   NC"商品コード"           TO      CSV1-K07.
     MOVE   NC"品単"                 TO      CSV1-K08.
     MOVE   NC"商品名１"             TO      CSV1-K09.
     MOVE   NC"商品名２"             TO      CSV1-K10.
     MOVE   NC"_番"                 TO      CSV1-K11.
     MOVE   NC"入荷予定数"           TO      CSV1-K12.
     MOVE   NC"メモ_"               TO      CSV1-K13.
     MOVE   NC"社外ＭＳＧ"           TO      CSV1-K14.
     MOVE   NC"ＭＳＧ名称"           TO      CSV1-K15.
     MOVE   NC"データ種別"           TO      CSV1-K16.
     MOVE   NC"Ｄ３６５伝票番号"     TO      CSV1-K17.
     MOVE   NC"商品カテゴリ"         TO      CSV1-K18.
     MOVE   NC"商品カテゴリ名称"     TO      CSV1-K19.
     WRITE  CSV-REC                   FROM   CSV1-REC.
*
 INIT-END.
     EXIT.
************************************************************
*      ■０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
*
     ADD        1           TO      NYYOTEIF-CNT.
*
*入荷予定リストＣＳＶ出力
*
     PERFORM  NYYOTECV-WRITE-SEC.
*
 MAIN-01.
     READ     NYYOTEIF
              AT END   MOVE  "END"  TO  END-FLG
              GO TO MAIN-END
     END-READ.
*
 MAIN-END.
     EXIT.
************************************************************
*      ■１      入荷予定リストＣＳＶ出力　                *
************************************************************
 NYYOTECV-WRITE-SEC       SECTION.
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
                                    CSV-MK19.
     MOVE     X"28"         TO      CSV-MS02 CSV-MS09 CSV-MS10
                                    CSV-MS15 CSV-MS19.
     MOVE     X"29"         TO      CSV-ME02 CSV-ME09 CSV-ME10
                                    CSV-ME15 CSV-ME19.
*カウントアップ
     ADD       1            TO      NYYOTECV-CNT.
*
*倉庫コード
     MOVE      NYY-F01      TO      CSV-M01.
*倉庫名
*  倉庫マスタ検索
     MOVE        SPACE      TO      SOK-REC.
     INITIALIZE                     SOK-REC.
     MOVE      NYY-F01      TO      SOK-F01.
     READ    ZSOKMS1
       INVALID
         MOVE ALL NC"＊"   TO       CSV-M02
       NOT INVALID
         MOVE    SOK-F02   TO       CSV-M02
     END-READ.
*仕入先コード
     MOVE      NYY-F02      TO      CSV-M03.
*発注_
     MOVE      NYY-F03      TO      CSV-M04.
*行_
     MOVE      NYY-F04      TO      CSV-M05.
*入荷予定日
     MOVE      NYY-F05(1:4) TO      CSV-M06(1:4).
     MOVE      "/"          TO      CSV-M06(5:1).
     MOVE      NYY-F05(5:2) TO      CSV-M06(6:2).
     MOVE      "/"          TO      CSV-M06(8:1).
     MOVE      NYY-F05(7:2) TO      CSV-M06(9:2).
*商品コード
     MOVE      NYY-F06      TO      CSV-M07.
*品単
     MOVE      NYY-F07      TO      CSV-M08.
*商品名１
*商品名２
*　商品名称マスタ検索
     MOVE        SPACE      TO      MEI-REC.
     INITIALIZE                     MEI-REC.
     MOVE      NYY-F06      TO      MEI-F011.
     MOVE      NYY-F07      TO      MEI-F012.
     READ    SUBMEIL1
       INVALID
         MOVE ALL NC"＊"   TO       CSV-M09
                                    CSV-M10
       NOT INVALID
         MOVE    MEI-F021  TO       CSV-M09
         MOVE    MEI-F022  TO       CSV-M10
     END-READ.
*_番
     MOVE      NYY-F08      TO      CSV-M11.
*入荷予定数
     MOVE      NYY-F09      TO      HENKAN1.
     IF        NYY-F09      <       0
               MOVE        "-"      TO      HENKAN1-DATA0
     ELSE
               MOVE        "0"      TO      HENKAN1-DATA0
     END-IF.
     MOVE      HENKAN1-1    TO      HENKAN1-DATA1.
     MOVE      "."          TO      HENKAN1-DATA2.
     MOVE      HENKAN1-2    TO      HENKAN1-DATA3.
     MOVE      HENKAN1-DATA TO      CSV-M12.
*メモ_
     MOVE      NYY-F10      TO      CSV-M13.
*社外ＭＳＧ
     MOVE      NYY-F111     TO      CSV-M14.
*ＭＳＧ名称
     MOVE      NYY-F112     TO      CSV-M15.
*データ種別
     MOVE      NYY-F12      TO      CSV-M16.
*Ｄ３６５伝票番号
     MOVE      NYY-F13      TO      CSV-M17.
*商品カテゴリ
     MOVE      NYY-F14      TO      CSV-M18.
*商品カテゴリ名称
*　条件ファイル検索
     MOVE        SPACE      TO      JYO-REC.
     INITIALIZE                     JYO-REC.
     MOVE      10           TO      JYO-F01.
     MOVE      NYY-F14      TO      JYO-F02.
     READ    JYOKEN1
       INVALID
         MOVE ALL NC"＊"   TO       CSV-M19
       NOT INVALID
         MOVE    JYO-F03   TO       CSV-M19
     END-READ.
*
*レコード出力
     WRITE     CSV-REC.

 NYYOTECV-WRITE-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE   NYYOTECV NYYOTEIF SUBMEIL1 ZSOKMS1 JYOKEN1.
*
     IF      NYYOTEIF-CNT NOT = ZERO
             DISPLAY  " NYYOTEIF-READ-CNT  = " NYYOTEIF-CNT
                                                    UPON CONS
             DISPLAY  " NYYOTECV-WRITE-CNT = " NYYOTECV-CNT
                                                    UPON CONS
     ELSE
             DISPLAY  " NYYOTEIF-READ-CNT  = " NYYOTEIF-CNT
                                                    UPON CONS
             DISPLAY  " NYYOTECV-WRITE-CNT = " NYYOTECV-CNT
                                                    UPON CONS
             MOVE    4010      TO     PROGRAM-STATUS
     END-IF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
