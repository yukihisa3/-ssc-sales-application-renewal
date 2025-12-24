******一時使用**************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　マスタ管理　　　                  *
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　マスタ更新履歴Ｆコンバート
*    作成日／作成者　　　：　2019/03/15 NAV                    *
*    処理概要　　　　　　：　S11300660                         *
*                              項目（Ｆ０９）追加              *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CNVMSTLG.
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
****<< マスタ更新履歴Ｆファイル >>******************************
     SELECT   MSTLOGF    ASSIGN  TO   DA-01-VS-MSTLOGF
                        ORGANIZATION         IS   SEQUENTIAL
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   IN-STATUS.
****<< ＣＮＶワークファイル >>******************************
     SELECT   MSTLOGW    ASSIGN  TO   DA-01-VS-MSTLOGW
                        ORGANIZATION         IS   SEQUENTIAL
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   WK-STATUS.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< マスタ更新履歴Ｆファイル >>******************************
 FD    MSTLOGF.
 01    IN-REC     PIC X(543).
****<< ＣＮＶワークファイル　　 >>******************************
 FD    MSTLOGW.
 01    WK-REC     PIC X(1055).
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 IN-STATUS           PIC  X(2).
     02 WK-STATUS           PIC  X(2).
****  カウンタ                ****
 01  I-CNT                   PIC  9(7)   VALUE  ZERO.
 01  O-CNT                   PIC  9(7)   VALUE  ZERO.
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
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
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CNVMSTLG".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*---------------- 99/09/28追加 --------------------------*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*--------------------------------------------------------*
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  MSTLOGF.
     MOVE   "MSTLOGF "       TO    ERR-FL-ID.
     MOVE    IN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  MSTLOGW.
     MOVE   "MSTLOGW "       TO    ERR-FL-ID.
     MOVE    WK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 CNVMSTLG-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 CNVMSTLG-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
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
     DISPLAY "CNVMSTLG START " HEN-DATE " " HEN-TIME UPON CONS.
*    DISPLAY HEN-DATE UPON CONS.
*    DISPLAY HEN-TIME UPON CONS.
     DISPLAY NC"マスタ更新履歴Ｆ　変換開始" UPON CONS.

     OPEN     INPUT     MSTLOGF.
     OPEN     OUTPUT    MSTLOGW.
     READ     MSTLOGF
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   INIT-END
     END-READ.
     ADD      1         TO     I-CNT.
*
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     MOVE     SPACE           TO      WK-REC.
     MOVE     IN-REC          TO      WK-REC.
     WRITE    WK-REC.
     ADD      1         TO     O-CNT.
     READ     MSTLOGF
              AT END    MOVE  "END"  TO  END-FLG
                        GO   TO   MAIN-END
     END-READ.
     ADD      1         TO     I-CNT.
 MAIN-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     DISPLAY NC"マスタ更新履歴Ｆ　読込件数＝" I-CNT UPON CONS.
     DISPLAY NC"マスタ更新履歴Ｆ　変換件数＝" O-CNT UPON CONS.
     CLOSE    MSTLOGF.
     DISPLAY NC"マスタ更新履歴Ｆ　変換終了" UPON CONS.
*システム日付取得
     ACCEPT    WK-TIME        FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)   TO     HEN-TIME-HH.
     MOVE      WK-TIME(3:2)   TO     HEN-TIME-MM.
     MOVE      WK-TIME(5:2)   TO     HEN-TIME-SS.
*
*    DISPLAY HEN-TIME UPON CONS.
     DISPLAY "CNVMSTLG END   " HEN-DATE " " HEN-TIME UPON CONS.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
