****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＸＣＥＬ連携                    *
*    モジュール名　　　　：　店舗横並発注集計データ出力        *
*    作成日／更新日　　　：　06/10/10                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　店舗横並データを読み、ＣＳＶＦに　*
*                          出力する。　　　　　　　　　　　　　*
*    作成日／更新日　　　：　20/07/17                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　在庫引当、小売連携、ＳＴＮＯ管理　*
*                          　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CSV0080B.
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
****<< 店舗横並発注集計データ >>******************************
     SELECT   CSVTBLF  ASSIGN    TO        DA-01-VI-CSVTBLL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   TBL-F01
                                                 TBL-F02
                                                 TBL-F03
                                                 TBL-F04
                                                 TBL-F05
                                                 TBL-F06
                        FILE      STATUS    IS   TBL-STATUS.
****<< 店舗順ワーク >>****************************************
     SELECT   SEQTENF  ASSIGN    TO        DA-01-VI-SEQTENL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   SET-F01
                        FILE      STATUS    IS   SET-STATUS.
****<< 発注集計データ >>**********************************
     SELECT   HACTECSV     ASSIGN    TO        DA-01-S-HACTECSV
                        FILE      STATUS    IS   HAC-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 店舗マスタ >>***************************************
 FD  CSVTBLF.
     COPY     CSVTBLF  OF        XFDLIB
              JOINING   TBL       PREFIX.
****<< 店舗順ワーク >>*************************************
 FD  SEQTENF.
     COPY     SEQTENF  OF        XFDLIB
              JOINING   SET       PREFIX.
****<< 発注集計データ >>***********************************
 FD  HACTECSV
                   BLOCK CONTAINS  1  RECORDS.
*
 01  HAC-REC.
     03  FILLER         PIC  X(3500).
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TBL-STATUS           PIC  X(02).
     02 SET-STATUS           PIC  X(02).
     02 HAC-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CSV0080B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  END-FLG1                PIC  X(03)  VALUE  SPACE.
 01  CSVTBLF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  SEQTENF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  IX                      PIC  9(03)  VALUE  ZERO.
 01  IY                      PIC  9(03)  VALUE  ZERO.
 01  OK-FLG                  PIC  X(02)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  RD-CNT              PIC  9(07)   VALUE  0.
     03  WRT-CNT             PIC  9(07)   VALUE  0.
     03  REW-CNT             PIC  9(07)   VALUE  0.
****  日付変換
 01  HIDUKE-HENKAN.
     03  HIDUKE-YYYY         PIC  X(04)   VALUE  SPACE.
     03  HIDUKE-FIL01        PIC  X(01)   VALUE  SPACE.
     03  HIDUKE-MM           PIC  X(02)   VALUE  SPACE.
     03  HIDUKE-FIL02        PIC  X(01)   VALUE  SPACE.
     03  HIDUKE-DD           PIC  X(02)   VALUE  SPACE.
****  時間変換
 01  JIKAN-HENKAN.
     03  JIKAN-HH            PIC  X(02)   VALUE  SPACE.
     03  JIKAN-FIL01         PIC  X(01)   VALUE  SPACE.
     03  JIKAN-MM            PIC  X(02)   VALUE  SPACE.
****  単価変換
 01  TANKA-HENKAN.
     03  TANKA-SURYO         PIC  X(07)   VALUE  SPACE.
     03  TANKA-FIL01         PIC  X(01)   VALUE  SPACE.
     03  TANKA-SYOSU         PIC  X(02)   VALUE  SPACE.
****  店舗格納ワーク          ****
 01  WK-TBL-TENPO.
     03  TBL-WORK            OCCURS  300.
         05  TBL-TENPOCD     PIC  9(05).
         05  TBL-TENPONM     PIC  N(05).
****  出力ファイル情報１
 01  DATA1-REC.
     03  FILLER            PIC  X(13)  VALUE ",,,,,,,,,,,,,".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(04)  VALUE NC"店舗情報".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  DATA1-TENPO-TBL.
         05  DATA1-TBL         OCCURS  242.
             07  DATA1-TENCD  PIC  9(05).
             07  DATA1-FIL01  PIC  X(01).
****  出力ファイル情報２
 01  DATA2-REC.
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(05)  VALUE NC"バッチ日付".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(05)  VALUE NC"バッチ時間".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(06)  VALUE NC"バッチ取引先".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(04)  VALUE NC"倉庫ＣＤ".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(03)  VALUE NC"納品日".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(07)  VALUE NC"量販店商品ＣＤ".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(06)  VALUE NC"サカタコード".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
*# 2020/07/17 NAV ST
*****03  FILLER            PIC  N(02)  VALUE NC"規格".
     03  FILLER            PIC  N(04)  VALUE NC"在庫引当".
*# 2020/07/17 NAV ED
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
*# 2020/07/17 NAV ST
*****03  FILLER            PIC  N(03)  VALUE NC"サイズ".
     03  FILLER            PIC  N(04)  VALUE NC"小売連携".
*# 2020/07/17 NAV ED
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
*# 2020/07/17 NAV ST
*****03  FILLER            PIC  N(02)  VALUE NC"入数".
     03  FILLER            PIC  N(06)  VALUE NC"ＳＴＮＯ管理".
*# 2020/07/17 NAV ED
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(04)  VALUE NC"商品名１".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(04)  VALUE NC"商品名２".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(04)  VALUE NC"原価単価".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(04)  VALUE NC"売価単価".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  DATA2-X.
         05  DATA2-TBL         OCCURS  242.
             07  DATA2-FIL02   PIC  X(01).
             07  DATA2-TENNM   PIC  N(05).
             07  DATA2-FIL03   PIC  X(01).
             07  DATA2-FIL01   PIC  X(01).
**** 出力ファイル情報３
 01  DATA3-REC.
     03  DATA3-F01         PIC  X(10).
     03  DATA3-A01         PIC  X(01).
     03  DATA3-F02         PIC  X(05).
     03  DATA3-A02         PIC  X(01).
     03  DATA3-F03         PIC  9(08).
     03  DATA3-A03         PIC  X(01).
     03  DATA3-F04         PIC  X(02).
     03  DATA3-A04         PIC  X(01).
     03  DATA3-F05         PIC  X(10).
     03  DATA3-A05         PIC  X(01).
     03  DATA3-F06         PIC  X(13).
     03  DATA3-A06         PIC  X(01).
     03  DATA3-F07         PIC  X(08).
     03  DATA3-A07         PIC  X(01).
*# 2020/07/17 NAV ST
*****03  DATA3-F08         PIC  X(06).
     03  DATA3-F081        PIC  X(01).
     03  DATA3-F082        PIC  N(01).
     03  DATA3-F083        PIC  X(01).
*# 2020/07/17 NAV ED
     03  DATA3-A08         PIC  X(01).
*# 2020/07/17 NAV ST
*****03  DATA3-F09         PIC  X(06).
     03  DATA3-F091        PIC  X(01).
     03  DATA3-F092        PIC  N(01).
     03  DATA3-F093        PIC  X(01).
*# 2020/07/17 NAV ED
     03  DATA3-A09         PIC  X(01).
*# 2020/07/17 NAV ST
*****03  DATA3-F10         PIC  X(04).
     03  DATA3-F101        PIC  X(01).
     03  DATA3-F102        PIC  N(01).
     03  DATA3-F103        PIC  X(01).
*# 2020/07/17 NAV ED
     03  DATA3-A10         PIC  X(01).
     03  DATA3-F11         PIC  X(15).
     03  DATA3-A11         PIC  X(01).
     03  DATA3-F12         PIC  X(15).
     03  DATA3-A12         PIC  X(01).
     03  DATA3-F13         PIC  9(10).
     03  DATA3-A13         PIC  X(01).
     03  DATA3-F14         PIC  X(10).
     03  DATA3-A14         PIC  X(01).
     03  DATA3-TBL         OCCURS  242.
         05  DATA3-SURYO   PIC  9(05).
         05  DATA3-FIL01   PIC  X(01).
*
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
***
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  CSVTBLF.
     MOVE   "CSVTBLL1"        TO    ERR-FL-ID.
     MOVE    TBL-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SEQTENF.
     MOVE   "SEQTENL1"        TO    ERR-FL-ID.
     MOVE    SET-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACTECSV.
     MOVE   "HACTECSV"        TO    ERR-FL-ID.
     MOVE    HAC-STATUS       TO    ERR-STCD.
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
     PERFORM           200-MAIN-SEC
             UNTIL     END-FLG   =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
*
     OPEN         INPUT     CSVTBLF.
     OPEN         OUTPUT    HACTECSV.
     OPEN         INPUT     SEQTENF.
*    店舗情報ワーク退避
     INITIALIZE             WK-TBL-TENPO.
     MOVE      SPACE    TO   DATA1-TENPO-TBL.
     MOVE      ZERO     TO   IX.
     MOVE      SPACE    TO   DATA2-X.
     PERFORM   TENPO-TBL-SEC  UNTIL  END-FLG1 = "END"
                                 OR  IX  >  242.
     IF    IX = ZERO
           MOVE  "END"  TO   END-FLG
           GO           TO   100-INIT-END
     END-IF.
*
     MOVE  SPACE        TO   HAC-REC.
     MOVE  DATA1-REC    TO   HAC-REC.
     WRITE HAC-REC.
     ADD   1            TO   WRT-CNT.
     MOVE  SPACE        TO   HAC-REC.
     MOVE  DATA2-REC    TO   HAC-REC.
     WRITE HAC-REC.
     ADD   1            TO   WRT-CNT.
*
     PERFORM CSVTBLF-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*情報セット
     MOVE   SPACE            TO   HAC-REC  DATA3-REC.
*    カンマセット
     MOVE   ","              TO   DATA3-A01 DATA3-A02 DATA3-A03.
     MOVE   ","              TO   DATA3-A04 DATA3-A05 DATA3-A06.
     MOVE   ","              TO   DATA3-A07 DATA3-A08 DATA3-A09.
     MOVE   ","              TO   DATA3-A10 DATA3-A11 DATA3-A12.
     MOVE   ","              TO   DATA3-A13 DATA3-A14.
*    バッチ日付
     MOVE   TBL-F01(1:4)     TO   HIDUKE-YYYY.
     MOVE   "/"              TO   HIDUKE-FIL01  HIDUKE-FIL02.
     MOVE   TBL-F01(5:2)     TO   HIDUKE-MM.
     MOVE   TBL-F01(7:2)     TO   HIDUKE-DD.
     MOVE   HIDUKE-HENKAN    TO   DATA3-F01.
*    バッチ時間
     MOVE   TBL-F02(1:2)     TO   JIKAN-HH.
     MOVE   TBL-F02(3:2)     TO   JIKAN-MM.
     MOVE   ":"              TO   JIKAN-FIL01.
     MOVE   JIKAN-HENKAN     TO   DATA3-F02.
*    バッチ取引先
     MOVE   TBL-F03          TO   DATA3-F03.
*    倉庫ＣＤ
     MOVE   TBL-F04          TO   DATA3-F04.
*    納品日
     MOVE   TBL-F05(1:4)     TO   HIDUKE-YYYY.
     MOVE   "/"              TO   HIDUKE-FIL01  HIDUKE-FIL02.
     MOVE   TBL-F05(5:2)     TO   HIDUKE-MM.
     MOVE   TBL-F05(7:2)     TO   HIDUKE-DD.
     MOVE   HIDUKE-HENKAN    TO   DATA3-F05.
*    量販店商品ＣＤ
     MOVE   TBL-F06          TO   DATA3-F06.
*    サカタ商品ＣＤ
     MOVE   TBL-F07          TO   DATA3-F07.
*# 2020/07/17 NAV ST
*****規格
**   MOVE   TBL-F08          TO   DATA3-F08.
*    サイズ
**   MOVE   TBL-F09          TO   DATA3-F09.
*    入数
**   MOVE   TBL-F10          TO   DATA3-F10.
     MOVE   X"28"    TO   DATA3-F081 DATA3-F091 DATA3-F101.
     MOVE   X"29"    TO   DATA3-F083 DATA3-F093 DATA3-F103.
*    在庫引当
     IF     TBL-F10(1:1)  = "1"
            MOVE NC"★"      TO   DATA3-F082
     ELSE
            MOVE SPACE       TO   DATA3-F082
     END-IF.
*    小売連携
     IF     TBL-F10(2:1)  NOT = SPACE
            MOVE NC"小"      TO   DATA3-F092
     ELSE
            MOVE SPACE       TO   DATA3-F092
     END-IF.
*    ＳＴＮＯ管理
     IF     TBL-F10(3:1)  = "1"
            MOVE NC"Ｓ"      TO   DATA3-F102
     ELSE
            MOVE SPACE       TO   DATA3-F102
     END-IF.
*# 2020/07/17 NAV ED
*    品名１
     MOVE   TBL-F111         TO   DATA3-F11.
*    品名２
     MOVE   TBL-F112         TO   DATA3-F12.
*    原価単価
     MOVE   TBL-F12(1:7)     TO   TANKA-SURYO.
     MOVE   "."              TO   TANKA-FIL01.
     MOVE   TBL-F12(8:2)     TO   TANKA-SYOSU.
     MOVE   TANKA-HENKAN     TO   DATA3-F13.
*    売価単価
     MOVE   TBL-F13(1:7)     TO   TANKA-SURYO.
     MOVE   "."              TO   TANKA-FIL01.
     MOVE   TBL-F13(8:2)     TO   TANKA-SYOSU.
     MOVE   TANKA-HENKAN     TO   DATA3-F14.
*    数量テーブルセット
     PERFORM VARYING IY FROM 1 BY 1 UNTIL  IY > IX
                                       OR  IY > 242
             MOVE  ","         TO DATA3-FIL01(IY)
             MOVE  TBL-F14(IY) TO DATA3-SURYO(IY)
     END-PERFORM.
     MOVE  DATA3-REC    TO   HAC-REC.
     WRITE HAC-REC.
     ADD   1            TO   WRT-CNT.
*
     PERFORM CSVTBLF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            店舗テーブルの読込処理
************************************************************
 TENPO-TBL-SEC                     SECTION.
*
     READ    SEQTENF
             AT  END  MOVE  "END"  TO  END-FLG1
             GO                    TO  TENPO-TBL-EXIT
     END-READ.
*
     ADD     1        TO     IX.
     MOVE    ","      TO     DATA1-FIL01(IX) DATA2-FIL01(IX).
     MOVE    SET-F01  TO     DATA1-TENCD(IX).
     MOVE    SET-F02  TO     DATA2-TENNM(IX).
*****DISPLAY "SET-F01    = " SET-F01     UPON CONS.
*****DISPLAY "SET-F02    = " SET-F02     UPON CONS.
     MOVE    X"28"    TO     DATA2-FIL02(IX).
     MOVE    X"29"    TO     DATA2-FIL03(IX).
*
 TENPO-TBL-EXIT.
     EXIT.
************************************************************
*    ＣＳＶ商品ファイル読込み
************************************************************
 CSVTBLF-READ-SEC                  SECTION.
*
     READ    CSVTBLF     AT  END
                         MOVE  "END"  TO END-FLG
             NOT AT END  ADD    1     TO RD-CNT
     END-READ.
*
 CSVTBLF-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             CSVTBLF  HACTECSV  SEQTENF.
*
     DISPLAY "* CSVTBLF  (INPUT) = " RD-CNT   " *"  UPON CONS.
     DISPLAY "* CSVDATA  (WRITE) = " WRT-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
