****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　ＲＨＣ（支払）データ変換処理　　　*
*    作成日／更新日　　　：　2008/12/05                        *
*    作成者／更新者　　　：　後閑　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＲＨＣにて受信したデータを変換し　*
*                            、ヘッダファイルと明細ファイルに　*
*                            セットする。　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSI7801B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/12/05.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*支払受信データファイル
     SELECT   ROYSIHA  ASSIGN    TO        DA-01-S-ROYSIHA
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    EDI-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*-支払ヘッダデータ
     SELECT   ROYSHEPF  ASSIGN         DA-01-VI-ROYSHEL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SIH-F03
                        STATUS         SIH-STATUS.
*支払明細データ
     SELECT   ROYSMEPF  ASSIGN         DA-01-VI-ROYSMEL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SIM-F01  SIM-F02
                                       SIM-F09  SIM-F04
                        STATUS         SIM-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２５６　  ＢＦ＝　１　
******************************************************************
 FD  ROYSIHA
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  EDI-REC.
     03  EDI-01                  PIC  X(02).
     03  EDI-02                  PIC  X(254).
******************************************************************
*    支払ヘッダデータ　ＲＬ＝　２５６
******************************************************************
 FD  ROYSHEPF
                        LABEL RECORD   IS   STANDARD.
     COPY     ROYSHEPF  OF        XFDLIB
              JOINING   SIH       PREFIX.
******************************************************************
*    支払明細データ　ＲＬ＝　２５５
******************************************************************
 FD  ROYSMEPF
                        LABEL RECORD   IS   STANDARD.
     COPY     ROYSMEPF  OF        XFDLIB
              JOINING   SIM       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  SIH-CNT                 PIC  9(08)     VALUE  ZERO.
 01  SIM-CNT                 PIC  9(08)     VALUE  ZERO.
 01  IX                      PIC  9(02)     VALUE  ZERO.
 01  JCA-CNT4                PIC  9(08)     VALUE  ZERO.
 01  TOKMS2-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  JHMRUTL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  WK-TOK-F81              PIC  X(02)     VALUE  SPACE.
 01  WK-JOH-F17              PIC  9(02)     VALUE  ZERO.
 01  WK-DENNO.
   03  WK-DENNO1             PIC  X(03)     VALUE  SPACE.
   03  WK-DENNO2             PIC  X(06)     VALUE  SPACE.
 01  WK-TEN                  PIC  X(04).
 01  WK-TEN-R     REDEFINES  WK-TEN.
   03  WK-TENCD              PIC  9(04).
 01  WK-DENKIN               PIC  X(12).
 01  WK-GOKEI                PIC  X(12).
 01  WK-GOKEI-R   REDEFINES  WK-GOKEI.
   03  GOKEIKIN              PIC  S9(11).
 01  WK-SIME-NEN             PIC  9(08)   VALUE  ZERO.
*
 01  COMP-DH-AREA.
     03  WRK-DH-KAI         PIC S9(10) VALUE  ZERO.
     03  WRK-DH-CHO         PIC S9(12) VALUE  ZERO.
     03  WRK-DH-SOI         PIC S9(12) VALUE  ZERO.
     03  WRK-DH-SIH         PIC S9(12) VALUE  ZERO.
     03  WRK-DH-ZEI         PIC S9(12) VALUE  ZERO.
 01  COMP-DD-AREA.
     03  WRK-DD-GEN         PIC S9(11) VALUE  ZERO.
     03  WRK-DD-DEN         PIC S9(11) VALUE  ZERO.
     03  WRK-DD-SIH         PIC S9(11) VALUE  ZERO.
     03  WRK-DD-ZEI         PIC S9(11) VALUE  ZERO.
*ヘッドレコード退避ワーク
 01  WK-SIDH-REC.
     03  WK-SIDH01          PIC  X(02).
     03  WK-SIDH02          PIC  9(08).
     03  WK-SIDH03          PIC  9(08).
     03  WK-SIDH04          PIC  9(08).
     03  WK-SIDH05          PIC  9(08).
     03  WK-SIDH06          PIC  9(08).
     03  WK-SIDH07          PIC  9(07).
     03  WK-SIDH08          PIC  9(10).
     03  WK-SIDH09          PIC  9(10).
     03  WK-SIDH10          PIC  9(10).
     03  WK-SIDH11          PIC  9(10).
     03  WK-SIDH12          PIC  9(10).
     03  WK-SIDH13          PIC  X(01).
     03  WK-SIDH14          PIC  9(12).
     03  WK-SIDH15          PIC  X(01).
     03  WK-SIDH16          PIC  9(11).
     03  WK-SIDH17          PIC  X(01).
     03  WK-SIDH18          PIC  9(11).
     03  WK-SIDH19          PIC  X(01).
     03  WK-SIDH20          PIC  9(12).
     03  WK-SIDH21          PIC  X(01).
     03  WK-SIDH22          PIC  9(09).
     03  WK-SIDH23          PIC  X(79).
     03  WK-SIDH24          PIC  9(11).
     03  WK-SIDH25          PIC  9(01).
     03  WK-SIDH26          PIC  9(06).
*    明細レコード退避ワーク
 01  WK-SIDD-REC.
     03  WK-SIDD01          PIC  X(02).
     03  WK-SIDD02          PIC  X(02).
     03  WK-SIDD03          PIC  9(08).
     03  WK-SIDD04          PIC  9(08).
     03  WK-SIDD05          PIC  9(08).
     03  WK-SIDD06          PIC  X(01).
     03  WK-SIDD07          PIC  9(11).
     03  WK-SIDD08          PIC  9(08).
     03  WK-SIDD09          PIC  9(08).
     03  WK-SIDD10          PIC  X(01).
     03  WK-SIDD11          PIC  9(11).
     03  WK-SIDD12          PIC  X(01).
     03  WK-SIDD13          PIC  9(11).
     03  WK-SIDD14          PIC  X(01).
     03  WK-SIDD15          PIC  9(09).
     03  WK-SIDD16          PIC  9(05).
     03  WK-SIDD17          PIC  9(03).
     03  WK-SIDD18          PIC  X(28).
     03  WK-SIDD19          PIC  9(01).
     03  WK-SIDD20          PIC  X(111).
     03  WK-SIDD21          PIC  9(11).
     03  WK-SIDD22          PIC  9(01).
     03  WK-SIDD23          PIC  9(06).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  SIH-STATUS         PIC   X(02).
     03  SIM-STATUS         PIC   X(02).
     03  EDI-STATUS         PIC   X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSI7801B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSI7801B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSI7801B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
 LINKAGE                SECTION.
 01  PARA-OUT-CNT1          PIC   9(07).
 01  PARA-OUT-CNT2          PIC   9(07).
 01  PARA-OUT-CNT3          PIC   9(07).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-OUT-CNT1
                                       PARA-OUT-CNT2
                                       PARA-OUT-CNT3.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ROYSIHA.
     MOVE      "ROYSIHA"    TO   AB-FILE.
     MOVE      EDI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ROYSHEPF.
     MOVE      "ROYSHEL1"   TO   AB-FILE.
     MOVE      SIH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ROYSMEPF.
     MOVE      "ROYSMEL1"   TO   AB-FILE.
     MOVE      SIM-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       ROYSIHA.
     OPEN     OUTPUT    ROYSHEPF.
     OPEN     OUTPUT    ROYSMEPF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        RD-CNT.
     MOVE     ZERO      TO        SIH-CNT.
     MOVE     ZERO      TO        SIM-CNT.
     MOVE     SPACE     TO        WK-SIDH-REC.
     INITIALIZE                   WK-SIDH-REC.
     MOVE     SPACE     TO        WK-SIDD-REC.
     INITIALIZE                   WK-SIDD-REC.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
*
     PERFORM  ROYSIHA-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*ファイルヘッダ処理
**** EDI-01 = "FL" の時は何もしない
*支払伝票ヘッダ処理
     IF    EDI-01         =    "DH"
           MOVE      SPACE       TO    WK-SIDH-REC
           INITIALIZE                  WK-SIDH-REC
*****ヘッダ情報→ワークにセット
           MOVE      EDI-REC     TO    WK-SIDH-REC
           PERFORM   MEISAI-HEAD-SEC
     END-IF.
*支払伝票明細処理
     IF   EDI-01          =    "DD"
*****明細情報→ワークにセット
          MOVE      EDI-REC     TO    WK-SIDD-REC
          PERFORM   MEISAI-SIHARAI-SEC
     END-IF.
*
     PERFORM ROYSIHA-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 ROYSIHA-READ-SEC      SECTION.
*
     MOVE "ROYSIHA-READ-SEC" TO       S-NAME.
*
     READ     ROYSIHA
              AT END
              MOVE     9      TO    END-FLG
              NOT AT END
              ADD      1      TO    RD-CNT
     END-READ.
*
 ROYSIHA-READ-EXIT.
     EXIT.
****************************************************************
*　　支払ヘッダレコード作成
****************************************************************
 MEISAI-HEAD-SEC    SECTION.
*
*****支払ヘッダ作成
*    対象期間自
     MOVE      WK-SIDH03   TO   SIH-F01.
*    対象期間至
     MOVE      WK-SIDH04   TO   SIH-F02.
*    締年月日（締年月日は明細用のワークに退避）
     MOVE      WK-SIDH05   TO   SIH-F03.
     MOVE      WK-SIDH05   TO   WK-SIME-NEN.
*    支払年月日
     MOVE      WK-SIDH06   TO   SIH-F04.
*    伝票枚数
     MOVE      WK-SIDH07   TO   SIH-F05.
*    当月取引額：納品
     MOVE      WK-SIDH08   TO   SIH-F06.
*    当月取引額：値引
     MOVE      WK-SIDH09   TO   SIH-F07.
*    当月取引額：値増
     MOVE      WK-SIDH10   TO   SIH-F08.
*    当月取引額：返品
     MOVE      WK-SIDH11   TO   SIH-F09.
*    当月取引額：税額
     MOVE      WK-SIDH12   TO   SIH-F10.
*    買掛計上予定額
     IF  WK-SIDH13         =    "-"
         MOVE  ZERO        TO   WRK-DH-KAI
         COMPUTE  WRK-DH-KAI =  WK-SIDH14 * -1
         MOVE  WRK-DH-KAI  TO   SIH-F11
     ELSE
         MOVE  WK-SIDH14   TO   SIH-F11
     END-IF.
*    調整額
     IF  WK-SIDH15         =    "-"
         MOVE  ZERO        TO   WRK-DH-CHO
         COMPUTE  WRK-DH-CHO =  WK-SIDH16 * -1
         MOVE  WRK-DH-CHO  TO   SIH-F12
     ELSE
         MOVE  WK-SIDH16   TO   SIH-F12
     END-IF.
*    相殺金額合計
     IF  WK-SIDH17         =    "-"
         MOVE  ZERO        TO   WRK-DH-SOI
         COMPUTE  WRK-DH-SOI =  WK-SIDH18 * -1
         MOVE  WRK-DH-SOI  TO   SIH-F13
     ELSE
         MOVE  WK-SIDH18   TO   SIH-F13
     END-IF.
*    支払金額合計
     IF  WK-SIDH19         =    "-"
         MOVE  ZERO        TO   WRK-DH-SIH
         COMPUTE  WRK-DH-SIH =  WK-SIDH20 * -1
         MOVE  WRK-DH-SIH  TO   SIH-F14
     ELSE
         MOVE  WK-SIDH20   TO   SIH-F14
     END-IF.
*    税額合計
     IF  WK-SIDH21         =    "-"
         MOVE  ZERO        TO   WRK-DH-ZEI
         COMPUTE  WRK-DH-ZEI =  WK-SIDH22 * -1
         MOVE  WRK-DH-ZEI  TO   SIH-F15
     ELSE
         MOVE  WK-SIDH22   TO   SIH-F15
     END-IF.
*
     MOVE       ZERO       TO   COMP-DH-AREA.
*
     WRITE  SIH-REC.
     ADD    1              TO   SIH-CNT.
*
 MEISAI-HEAD-EXIT.
     EXIT.
****************************************************************
*　　支払明細レコード作成
****************************************************************
 MEISAI-SIHARAI-SEC SECTION.
*****支払明細作成
*    締年月日
     MOVE      WK-SIME-NEN TO   SIM-F01.
*    店舗ＣＤ
     MOVE      WK-SIDD16   TO   SIM-F02.
*    店舗名
     MOVE      WK-SIDD18   TO   SIM-F03.
*    伝票番号
     MOVE      WK-SIDD03   TO   SIM-F04.
*    伝票区分
     MOVE      WK-SIDD02   TO   SIM-F05.
*    発注日
     MOVE      WK-SIDD04   TO   SIM-F06.
*    納品日
     MOVE      WK-SIDD05   TO   SIM-F07.
*    受領日
     MOVE      WK-SIDD08   TO   SIM-F08.
*    検収日
     MOVE      WK-SIDD09   TO   SIM-F09.
*    原価金額
     IF  WK-SIDD06         =    "-"
         MOVE  ZERO        TO   WRK-DD-GEN
         COMPUTE  WRK-DD-GEN =  WK-SIDD07 * -1
         MOVE  WRK-DD-GEN  TO   SIM-F10
     ELSE
         MOVE  WK-SIDD07   TO   SIM-F10
     END-IF.
*    伝票合計金額
     IF  WK-SIDD10         =    "-"
         MOVE  ZERO        TO   WRK-DD-DEN
         COMPUTE  WRK-DD-DEN =  WK-SIDD11 * -1
         MOVE  WRK-DD-DEN  TO   SIM-F11
     ELSE
         MOVE  WK-SIDD11   TO   SIM-F11
     END-IF.
*    支払金額
     IF  WK-SIDD12         =    "-"
         MOVE  ZERO        TO   WRK-DD-SIH
         COMPUTE  WRK-DD-SIH =  WK-SIDD13 * -1
         MOVE  WRK-DD-SIH  TO   SIM-F12
     ELSE
         MOVE  WK-SIDD13   TO   SIM-F12
     END-IF.
*    税額
     IF  WK-SIDD14         =    "-"
         MOVE  ZERO        TO   WRK-DD-ZEI
         COMPUTE  WRK-DD-ZEI =  WK-SIDD15 * -1
         MOVE  WRK-DD-ZEI  TO   SIM-F13
     ELSE
         MOVE  WK-SIDD15   TO   SIM-F13
     END-IF.
*
     MOVE       ZERO       TO   COMP-DD-AREA.
*
     WRITE  SIM-REC.
     ADD    1              TO   SIM-CNT.
 MEISAI-SIHARAI-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     CLOSE     ROYSIHA   ROYSHEPF   ROYSMEPF.
*
     DISPLAY NC"＃ＲＥＡＤ　ＣＮＴ＝"  RD-CNT    UPON CONS.
     DISPLAY NC"＃ＯＵＴ　ＨＥ　ＣＮＴ＝"  SIH-CNT   UPON CONS.
     DISPLAY NC"＃ＯＵＴ　ＭＥ　ＣＮＴ＝"  SIM-CNT   UPON CONS.
     MOVE     RD-CNT       TO   PARA-OUT-CNT1.
     MOVE     SIH-CNT      TO   PARA-OUT-CNT2.
     MOVE     SIM-CNT      TO   PARA-OUT-CNT3.
     DISPLAY  MSG-END UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
