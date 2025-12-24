****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　流通ＢＭＳ　　　　　　　　　　　　*
*    業務名　　　　　　　：　返品　　　　　　　　　　　　　　　*
*    モジュール名　　　　：　返品明細ＣＳＶ出力（エンチョー）  *
*    作成日／作成者　　　：　ARK 2023/11/31                    *
*    処理概要　　　　　　：　エンチョー返品データより　　　　　*
*                            返品明細ＣＳＶを出力する。　　　　*
*    更新日／更新者　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY2750V.
*                  流用:SBM0167B（山新）
 AUTHOR.                NAV.
 DATE-WRITTEN.          2023/11/31.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*返品伝票データ
     SELECT   ECHENPF   ASSIGN    TO        DA-01-VI-ECHENPL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HEP-F024 HEP-F026
                                            HEP-F034
                        FILE  STATUS   IS   HEP-STS.
*返品データ
     SELECT   ECHEPCSV  ASSIGN    TO        DA-01-VS-ECHEPCSV
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   HEW-STS.
*店舗マスタ
     SELECT  HTENMS     ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE  STATUS   IS   TEN-STS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    返品伝票データ
******************************************************************
 FD  ECHENPF            LABEL RECORD   IS   STANDARD.
     COPY     ECHENPF   OF        XFDLIB
              JOINING   HEP       PREFIX.
******************************************************************
*    返品データ
******************************************************************
 FD  ECHEPCSV           LABEL RECORD   IS   STANDARD.
     COPY     ECHEPCSV  OF        XFDLIB
              JOINING   HEW       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  READ-CNT                PIC  9(07)     VALUE  ZERO.
 01  WRITE-CNT               PIC  9(07)     VALUE  ZERO.
 01  UNMACH-CNT              PIC  9(07)     VALUE  ZERO.
*
*システム日付の編集
 01  SYS-WORKAREA.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*
 01  STS-AREA.
     03  HEP-STS           PIC  X(02).
     03  HEW-STS           PIC  X(02).
*
     03  TEN-STS           PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY2750V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY2750V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY2750V".
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
*
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ECHENPF.
     MOVE      "ECHENPL1"   TO   AB-FILE.
     MOVE      HEP-STS      TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ECHEPCSV.
     MOVE      "ECHEPCSV"   TO   AB-FILE.
     MOVE      HEW-STS      TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE      "TENMS1"    TO   AB-FILE.
     MOVE      TEN-STS     TO   AB-STS.
     DISPLAY   MSG-ABEND        UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     DISPLAY   ABEND-FILE       UPON CONS.
     MOVE      4000        TO   PROGRAM-STATUS.
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
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
*
     MOVE     SPACE               TO   HEW-REC.
     INITIALIZE                        HEW-REC.
     OPEN     INPUT     ECHENPF
                        HTENMS.
     OPEN     OUTPUT    ECHEPCSV.
*
     DISPLAY  MSG-START UPON CONS.
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
*ファイル読込
     PERFORM INREAD-SEC.
     IF   END-FLG = "END"
      DISPLAY NC"＃＃　返品伝票データ無　＃＃"  UPON CONS
          GO                    TO   INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
*
****************************************************************
*   返品伝票データ読込み
****************************************************************
 INREAD-SEC                 SECTION.
*
     MOVE    "INREAD-SEC"   TO   S-NAME.
*
     READ     ECHENPF
         AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   INREAD-EXIT
     END-READ.
     ADD     1    TO   READ-CNT.
*
 INREAD-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*返品データ出力
*
     PERFORM  HEW-WRITE-SEC.
*返品伝票データ読込
     PERFORM  INREAD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数出力
*返品伝票データ読込
     DISPLAY "ECHENPF   READ CNT  = " READ-CNT UPON CONS.
*返品データ出力
     DISPLAY "ECHEPCSV  WRITE CNT = " WRITE-CNT UPON CONS.
*
     CLOSE    ECHENPF   ECHEPCSV   HTENMS.
*
     DISPLAY  MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　レコード出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 HEW-WRITE-SEC         SECTION.
*
     MOVE    "HEW-WRITE-SEC"     TO   S-NAME.
*
     MOVE     SPACE               TO   HEW-REC.
     INITIALIZE                        HEW-REC.
     MOVE     HEP-F014       TO  HEW-F01.
     MOVE     HEP-F024       TO  HEW-F02.
*
     PERFORM  TEN-READ.
     MOVE     TEN-F03        TO  HEW-F03.
     MOVE     HEP-F026       TO  HEW-F04.
     MOVE     HEP-F029       TO  HEW-F05.
     MOVE     HEP-F02A       TO  HEW-F06.
     MOVE     HEP-F02E       TO  HEW-F07.
     MOVE     HEP-F04        TO  HEW-F08.
     MOVE     HEP-F05        TO  HEW-F09.
     MOVE     HEP-F06        TO  HEW-F10.
     MOVE     HEP-F034       TO  HEW-F11.
     MOVE     HEP-F036       TO  HEW-F12.
     MOVE     HEP-F037       TO  HEW-F13.
     MOVE     HEP-F038       TO  HEW-F14.
     MOVE     HEP-F039       TO  HEW-F15.
     MOVE     HEP-F03A       TO  HEW-F16.
     MOVE     HEP-F03B       TO  HEW-F17.
     COMPUTE  HEW-F18        =   HEW-F16 * HEW-F17.
     MOVE     HEP-F03C       TO  HEW-F19.
*
     WRITE   HEW-REC.
     ADD     1      TO      WRITE-CNT.
*
 HPW-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　店舗マスタ読込　　　　　　　　　　　　　　　　　
*************************************************
 TEN-READ         SECTION.
** 店舗マスタ
     MOVE    HEP-F024      TO   TEN-F011.
     MOVE    HEP-F014      TO   TEN-F52.
     READ  HTENMS
       INVALID
         MOVE   ALL NC"＊"  TO   TEN-F03
     END-READ.
*
 TEN-READ-EXIT.
     EXIT.

