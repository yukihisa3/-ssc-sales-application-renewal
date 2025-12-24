****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫システム改善　　　　　　　　　*
*    モジュール名　　　　：　実績系Ｆ制御ファイル更新　　　　　*
*    作成日／更新日　　　：　09/10/27                          *
*    作成者／更新者　　　：　ＮＡＶ　大野　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタより処理日付・時刻、データ*
*　　　　　　　　　　　　　　件数を受け取り、実績系Ｆ制御ファイ*
*　　　　　　　　　　　　　　ルを処理日付・処理時刻のキーで読込*
*　　　　　　　　　　　　　　データ件数、終了日付・時刻、　　　*
*　　　　　　　　　　　　　　結果ＦＬＧを更新する。　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SPD9991B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<　　　　　商品名称マスタ　　　　　　　 >>*****************
     SELECT   JISPCCF   ASSIGN    TO        DA-01-VI-JISPCCL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JIS-F01
                                                 JIS-F02
                        FILE      STATUS    IS   JIS-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 商品名称マスタ　　　　　　　　　　>>********************
 FD  JISPCCF.
     COPY     JISPCCL1  OF        XFDLIB
              JOINING   JIS       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 JIS-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SPD9991B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  フラグ                  ****
 01  FLGS.
       03  END-FLG           PIC  X(03)  VALUE  SPACE.
       03  INV-FLG           PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)     VALUE  0.
     03  OUT-CNT             PIC  9(07)     VALUE  0.
*
****システム日付　　　　      ****
 01  SYSDATE                 PIC  9(08)     VALUE  ZERO.
 01  SYSTIME.
     03  SYS-HHMMSS              PIC  9(06)     VALUE  ZERO.
     03  SYS-MS                  PIC  9(02)     VALUE  ZERO.
****日付変換サブルーチン用ワーク*****
 01  LINK-IN-KBN             PIC  X(01).
 01  LINK-IN-YMD6            PIC  9(06).
 01  LINK-IN-YMD8            PIC  9(08).
 01  LINK-OUT-RET            PIC  X(01).
 01  LINK-OUT-YMD            PIC  9(08).
****  日付取得                ****
 01  WK-DATE8                PIC  9(08).
 01  WK-TIME6                PIC  9(06).
*
 LINKAGE                            SECTION.
 01  PARA-SDATE              PIC 9(08).
 01  PARA-STIME              PIC 9(06).
 01  PARA-DATAK              PIC 9(09).
************************************************************
 PROCEDURE              DIVISION  USING     PARA-SDATE
                                            PARA-STIME
                                            PARA-DATAK.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  JISPCCF.
     MOVE   "JISPCCF"         TO    ERR-FL-ID.
     MOVE    JIS-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC.
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*     100     初期処理
************************************************************
 100-INIT-SEC                     SECTION.
*
     ACCEPT   SYSDATE        FROM      DATE.
     ACCEPT   SYSTIME        FROM      TIME.
*
     MOVE     "3"            TO        LINK-IN-KBN.
     MOVE     SYSDATE        TO        LINK-IN-YMD6.
     MOVE     ZERO           TO        LINK-IN-YMD8.
     MOVE     ZERO           TO        LINK-OUT-RET.
     MOVE     ZERO           TO        LINK-OUT-YMD.
     CALL     "SKYDTCKB"     USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD   TO        WK-DATE8.
*
     MOVE     SYS-HHMMSS     TO        WK-TIME6.
*
     DISPLAY "* PARA-SDATE = " PARA-SDATE " *"  UPON CONS.
     DISPLAY "* PARA-STIME = " PARA-STIME " *"  UPON CONS.
     DISPLAY "* PARA-DATAK = " PARA-DATAK " *"  UPON CONS.
*
     OPEN     I-O       JISPCCF.
*
     INITIALIZE         FLGS.
*
 100-INIT-END.
     EXIT.
************************************************************
*     200     主処理
************************************************************
 200-MAIN-SEC                     SECTION.
*
***<   実績系Ｆ制御ファイルの読み込み   >***
*     パラメタの処理日付・処理時刻をキーにセット
      MOVE      PARA-SDATE          TO   JIS-F01.
      MOVE      PARA-STIME          TO   JIS-F02.
*
      READ    JISPCCF
*         存在しない時はレコード再作成
          INVALID
*             実績系Ｆ制御ファイルレコードの初期化
              MOVE      SPACE       TO   JIS-REC
              INITIALIZE                 JIS-REC
*
              MOVE      WK-DATE8    TO   JIS-F01
              MOVE      WK-TIME6    TO   JIS-F02
              MOVE      PARA-DATAK  TO   JIS-F03
              MOVE      WK-DATE8    TO   JIS-F04
              MOVE      WK-TIME6    TO   JIS-F05
              MOVE     "E"          TO   JIS-F06
              MOVE     "INV"        TO   INV-FLG
*         存在した時は更新
          NOT INVALID
              MOVE      PARA-DATAK  TO   JIS-F03
              MOVE      WK-DATE8    TO   JIS-F04
              MOVE      WK-TIME6    TO   JIS-F05
              MOVE     "E"          TO   JIS-F06
              MOVE      SPACE       TO   INV-FLG
      END-READ.
*レコードの更新・作成処理
      IF      INV-FLG  =  "INV"
              WRITE                      JIS-REC
      ELSE
              REWRITE                    JIS-REC
      END-IF.
*
      ADD     1              TO        OUT-CNT.
*
 200-MAIN-SEC-EXT.
     EXIT.
***********************************************************
*      300    終了処理
***********************************************************
 300-END-SEC                     SECTION.
      CLOSE                      JISPCCF.
*
      DISPLAY "* JISPCCF(OUTPUT) = " OUT-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*
*****************<<  PROGRAM  END  >>***********************
