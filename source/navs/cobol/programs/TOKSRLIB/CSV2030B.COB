****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷                              *
*    モジュール名　　　　：　新発注集計データＣＳＶ出力        *
*    作成日　　　　　　　：　2022/05/12                        *
*    作成者　　　　　　　：　NAV INOUE                         *
*    処理概要　　　　　　：　新発注集計データを　　　　　　　　*
*                          　ＣＳＶ形式に編集する。　　　　　　*
*    更新日　　　　　　　：　2022/05/19                        *
*    更新者　　　　　　　：　NAV INOUE                         *
*    処理概要　　　　　　：　新発注集計ＣＳＶへの転送項目追加　*
*                          　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CSV2030B.
*                  流用:CSV0130B
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
*新発注集計データ
     SELECT   NHTMEIF   ASSIGN         NHTMEIL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE IS   SEQUENTIAL
                        RECORD    KEY  IS   HME-F01
                                            HME-F02
                                            HME-F03
                                            HME-F04
                                            HME-F05
                                            HME-F06
                                            HME-F15
                        STATUS         IS   HME-STATUS.
*新発注集計店舗データ
     SELECT   NHTTENF   ASSIGN         NHTTENL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE IS   SEQUENTIAL
                        RECORD    KEY  IS   HTE-F01
                        STATUS         IS   HTE-STATUS.
*新発注集計ＣＳＶ
     SELECT   NHTMEICV  ASSIGN    TO        DA-01-S-NHTMEICV
                        ORGANIZATION   SEQUENTIAL
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    IS   MEI-STATUS.
*新発注集計店舗ＣＳＶ
     SELECT   NHTTENCV  ASSIGN    TO        DA-01-S-NHTTENCV
                        ORGANIZATION   SEQUENTIAL
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    IS   TEN-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
*新発注集計データ
 FD  NHTMEIF            LABEL     RECORD   IS   STANDARD.
                        COPY      NHTMEIF  OF   XFDLIB
                        JOINING   HME      AS   PREFIX.
*新発注集計店舗データ
 FD  NHTTENF            LABEL     RECORD   IS   STANDARD.
                        COPY      NHTTENF  OF   XFDLIB
                        JOINING   HTE      AS   PREFIX.
*新発注集計ＣＳＶ
 FD  NHTMEICV           LABEL     RECORD   IS   STANDARD
*↓2022/05/19
*                       BLOCK     CONTAINS 13   RECORDS.
                        BLOCK     CONTAINS 12   RECORDS.
*↑2022/05/19
                        COPY      NHTMEICV OF   XFDLIB
                        JOINING   MEI      AS   PREFIX.
*新発注集計店舗ＣＳＶ
 FD  NHTTENCV           LABEL     RECORD   IS   STANDARD
                        BLOCK     CONTAINS 116  RECORDS.
                        COPY      NHTTENCV OF   XFDLIB
                        JOINING   TEN      AS   PREFIX.
*
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 HME-STATUS           PIC  X(02).
     02 HTE-STATUS           PIC  X(02).
     02 MEI-STATUS           PIC  X(02).
     02 TEN-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "CSV2030B".
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
 01  CSSTBLF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  SEQTENF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  IX                      PIC  9(03)  VALUE  ZERO.
 01  IY                      PIC  9(03)  VALUE  ZERO.
 01  OK-FLG                  PIC  X(02)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  RD-CNT1             PIC  9(07)   VALUE  0.
     03  RD-CNT2             PIC  9(07)   VALUE  0.
     03  WRT-CNT1            PIC  9(07)   VALUE  0.
     03  WRT-CNT2            PIC  9(07)   VALUE  0.
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
*
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
***
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NHTMEIF.
     MOVE   "NHTMEIL1"        TO    ERR-FL-ID.
     MOVE    HME-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NHTTENF.
     MOVE   "NHTTENL1"        TO    ERR-FL-ID.
     MOVE    HTE-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NHTMEICV.
     MOVE   "NHTMEICV"        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NHTTENCV.
     MOVE   "NHTTENCV"        TO    ERR-FL-ID.
     MOVE    TEN-STATUS       TO    ERR-STCD.
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
     PERFORM    100-INIT-SEC.
     PERFORM    200-MAIN-SEC  UNTIL  END-FLG = "END".
     MOVE       SPACE         TO     END-FLG.
     PERFORM    210-MAIN-SEC  UNTIL  END-FLG = "END".
     PERFORM    300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
*
     OPEN         INPUT     NHTMEIF  NHTTENF.
     OPEN         OUTPUT    NHTMEICV NHTTENCV.
*
*新発注集計データＲＥＡＤ
     PERFORM  900-HME-READ.
     IF       END-FLG  =  "END"
              DISPLAY  NC"　＃対象データなし．＃"  UPON CONS
              MOVE     4010       TO   PROGRAM-STATUS
              GO                  TO   100-INIT-END
     END-IF.
*新発注集計店舗データＲＥＡＤ
     PERFORM  900-HTE-READ.
     IF       END-FLG  =  "END"
              DISPLAY  NC"　＃対象データなし．．＃"  UPON CONS
              MOVE     4010       TO   PROGRAM-STATUS
              GO                  TO   100-INIT-END
     END-IF.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理１　新発注集計ＣＳＶ出力             *
************************************************************
 200-MAIN-SEC           SECTION.
*情報セット
     MOVE   SPACE            TO   MEI-REC.
*
     MOVE   ","              TO   MEI-MK01  MEI-MK02  MEI-MK03
                                  MEI-MK04  MEI-MK05  MEI-MK06
                                  MEI-MK07  MEI-MK08  MEI-MK09
                                  MEI-MK10  MEI-MK11  MEI-MK12
                                  MEI-MK13  MEI-MK14  MEI-MK15
                                  MEI-MK16  MEI-MK17  MEI-MK18
                                  MEI-MK19  MEI-MK20  MEI-MK21
                                  MEI-MK22  MEI-MK23  MEI-MK24
                                  MEI-MK25  MEI-MK26  MEI-MK27
*↓2022/05/19
*                                 MEI-MK28  MEI-MK29  MEI-MK30.
                                  MEI-MK28  MEI-MK29  MEI-MK30
                                  MEI-MK31  MEI-MK32.
*↑2022/05/19
*
     MOVE   X"28"            TO   MEI-MS20  MEI-MS21.
     MOVE   X"29"            TO   MEI-ME20  MEI-ME21.
*
*    バッチ日付
     MOVE   HME-F01(1:4)     TO   MEI-M01(1:4).
     MOVE   "/"              TO   MEI-M01(5:1).
     MOVE   HME-F01(5:2)     TO   MEI-M01(6:2).
     MOVE   "/"              TO   MEI-M01(8:1).
     MOVE   HME-F01(7:2)     TO   MEI-M01(9:2).
*    バッチ時間
     MOVE   HME-F02(1:2)     TO   MEI-M02(1:2).
     MOVE   ":"              TO   MEI-M02(3:1).
     MOVE   HME-F02(3:2)     TO   MEI-M02(4:2).
*    バッチ取引先
     MOVE   HME-F03          TO   MEI-M03.
*    倉庫ＣＤ
     MOVE   HME-F04          TO   MEI-M04.
*    納品日
     MOVE   HME-F05(1:4)     TO   MEI-M05(1:4).
     MOVE   "/"              TO   MEI-M05(5:1).
     MOVE   HME-F05(5:2)     TO   MEI-M05(6:2).
     MOVE   "/"              TO   MEI-M05(8:1).
     MOVE   HME-F05(7:2)     TO   MEI-M05(9:2).
*    量販店商品ＣＤ
     MOVE   HME-F06          TO   MEI-M06.
*↓2022/05/19
*    Ｄ３６５用ＪＡＮＣＤ
     MOVE   HME-F31          TO   MEI-M32.
*↑2022/05/19
*    サカタ商品ＣＤ
     MOVE   HME-F07          TO   MEI-M07.
*    サカタ品単ＣＤ
     MOVE   HME-F08          TO   MEI-M08.
*
*****規格
**   MOVE   HME-F09          TO   MEI-M09.
*    サイズ
**   MOVE   HME-F10          TO   MEI-M10.
*    入数
*****MOVE   HME-F11          TO   MEI-M11.
*
*    品名１
     MOVE   HME-F121         TO   MEI-M12.
*    品名２
     MOVE   HME-F122         TO   MEI-M13.
*    原価単価
     MOVE   HME-F13          TO   MEI-M14.
*    売価単価
     MOVE   HME-F14          TO   MEI-M15.
*    店舗コード
     MOVE   HME-F15          TO   MEI-M16.
*    店舗順番
     MOVE   HME-F16          TO   MEI-M17.
*    受注時数量
     MOVE   HME-F17          TO   MEI-M18.
*    訂正後数量
     MOVE   HME-F18          TO   MEI-M19.
*    商品名漢字１
     MOVE   HME-F19          TO   MEI-M20.
*    商品名漢字２
     MOVE   HME-F20          TO   MEI-M21.
*    在庫引当区分
     MOVE   HME-F21          TO   MEI-M22.
*    小売連携
     MOVE   HME-F22          TO   MEI-M23.
*    ＳＴＮＯ管理
     MOVE   HME-F23          TO   MEI-M24.
*↓2022/05/19
*    商品カテゴリ
     MOVE   HME-F30          TO   MEI-M31.
*↑2022/05/19
*    出力区分
     MOVE   HME-F24          TO   MEI-M25.
*    予備項目１
     MOVE   HME-F25          TO   MEI-M26.
*    予備項目２
     MOVE   HME-F26          TO   MEI-M27.
*    予備項目３
     MOVE   HME-F27          TO   MEI-M28.
*    予備項目４
     MOVE   HME-F28          TO   MEI-M29.
*    予備項目５
     MOVE   HME-F29          TO   MEI-M30.
*
     WRITE  MEI-REC.
     ADD    1            TO   WRT-CNT1.
*
     PERFORM  900-HME-READ.
*
 200-MAIN-SEC-EXT.
     EXIT.
****************************************************************
*    LEVEL ALL    新発注集計データ　 READ                      *
****************************************************************
 900-HME-READ           SECTION.
*    MOVE     "900-HME-READ"      TO   S-NAME.
*
     READ     NHTMEIF
         AT END
              MOVE     "END"      TO   END-FLG
              GO        TO        900-HME-READ-EXIT
     END-READ.
*
     ADD      1         TO        RD-CNT1.
*
 900-HME-READ-EXIT.
     EXIT.
************************************************************
*      ２１０   主処理２　新発注集計店舗ＣＳＶ出力         *
************************************************************
 210-MAIN-SEC           SECTION.
*情報セット
     MOVE   SPACE            TO   TEN-REC.
*
     MOVE   ","              TO   TEN-MK01  TEN-MK02  TEN-MK03.
*
     MOVE   X"28"            TO   TEN-MS02.
     MOVE   X"29"            TO   TEN-ME02.
*
*    店舗ＣＤ
     MOVE   HTE-F01          TO   TEN-M01.
*    店舗名
     MOVE   HTE-F02          TO   TEN-M02.
*    店舗順番
     MOVE   HTE-F03          TO   TEN-M03.
*
     WRITE  TEN-REC.
     ADD    1            TO   WRT-CNT2.
*
     PERFORM  900-HTE-READ.
*
 210-MAIN-SEC-EXT.
     EXIT.
****************************************************************
*    LEVEL ALL    新発注集計店舗データ　 READ                  *
****************************************************************
 900-HTE-READ           SECTION.
*    MOVE     "900-HTE-READ"      TO   S-NAME.
*
     READ     NHTTENF
         AT END
              MOVE     "END"      TO   END-FLG
              GO        TO        900-HTE-READ-EXIT
     END-READ.
*
     ADD      1         TO        RD-CNT2.
*
 900-HTE-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
*
     CLOSE      NHTMEIF NHTTENF NHTMEICV NHTTENCV.
*
     DISPLAY "* NHTMEIF  (INPUT) = " RD-CNT1   " *"  UPON CONS.
     DISPLAY "* NHTMEICV (WRITE) = " WRT-CNT1  " *"  UPON CONS.
     DISPLAY "* NHTTENF  (INPUT) = " RD-CNT2   " *"  UPON CONS.
     DISPLAY "* NHTTENCV (WRITE) = " WRT-CNT2  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
