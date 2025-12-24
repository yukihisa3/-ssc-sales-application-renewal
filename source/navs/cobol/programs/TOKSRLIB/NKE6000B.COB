****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷検品　　　　　　　            *
*    モジュール名　　　　：　出荷データエラー分抽出　　　　　　*
*    処理概要　　　　　　：　検品結果集計ファイル（カインズ）　*
*                            からチェック結果(F17)="1"         *
*                            のデータを抽出する。　　　　　　　*
*    流用　　　　　　　　：　NKE6000B                          *
*    作成日／作成者　　　：　2024/04/22 NAV                    *
*    更新日／更新者　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE6000B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2024/04/22.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA        IS   YA
     YB        IS   YB
     YA-22     IS   YA-22
     YB-22     IS   YB-22
     YB-21     IS   YB-21
     YA-21     IS   YA-21
     STATION   IS   STAT
     CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*検品結果集計ファイル
     SELECT   CZSUMTK1  ASSIGN    TO        DA-01-VI-CZSUMTK1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SUM-F01
                                            SUM-F02
                                            SUM-F03
                                            SUM-F04
                                            SUM-F05
                                            SUM-F06
                                            SUM-F07
                                            SUM-F08
                                            SUM-F09
                                            SUM-F10
                                            SUM-F11
                        FILE  STATUS   IS   SUM-STATUS.
*検品結果抽出ファイル（カインズ）
     SELECT    CZSUMWWF ASSIGN    TO       DA-01-VS-CZSUMWWF
                        ORGANIZATION       SEQUENTIAL
                        ACCESS    MODE     SEQUENTIAL
                        FILE      STATUS   SMW-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*検品結果集計ファイル
 FD  CZSUMTK1            LABEL RECORD   IS   STANDARD.
     COPY     CZSUMTK1    OF        XFDLIB
     JOINING  SUM       PREFIX.
*検品結果抽出ファイル（カインズ）
 FD  CZSUMWWF             LABEL RECORD   IS   STANDARD.
     COPY     CZSUMWWF    OF        XFDLIB
     JOINING  SMW       AS        PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  CZSUMTK1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  CZSUMTK1-ERR-CNT        PIC  9(07)     VALUE  ZERO.
 01  CZSUMWWF-INV-FLG        PIC  X(03)     VALUE  SPACE.
*取込日付／時刻バックアップ
 01  WK-KEY.
     03  WK-TRDATE           PIC  9(08)     VALUE  ZERO.
     03  WK-TRTIME           PIC  9(06)     VALUE  ZERO.
*システム日付の編集
 01  WK-SYS-DATE.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*ステータス
 01  WK-ST.
     03  SUM-STATUS        PIC  X(02).
     03  SMW-STATUS        PIC  X(02).
*BRK項目　エリア
*    03  BRK-SUM-F95       PIC  9(06)    VALUE ZERO.
*    03  BRK-SUM-F95-FLG   PIC  X(03)    VALUE SPACE.
*    03  BRK-SUM-F03       PIC  9(08)    VALUE ZERO.
*    03  BRK-SUM-F03-FLG   PIC  X(03)    VALUE SPACE.
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NKE6000B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE6000B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE6000B".
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
*時刻編集
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*LINKAGE                SECTION.
*01  PARA-IN-BUMON         PIC   X(04).
*01  PARA-IN-TANCD         PIC   X(02).
*01  PARA-IN-SOKCD         PIC   X(02).
*01  PARA-IN-CKUBUN        PIC   X(01).
*01  PARA-IN-TDATE         PIC   9(08).
*01  PARA-IN-TTIME         PIC   9(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*                                USING PARA-IN-BUMON
*                                      PARA-IN-TANCD
*                                      PARA-IN-SOKCD
*                                      PARA-IN-CKUBUN
*                                      PARA-IN-TDATE
*                                      PARA-IN-TTIME.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CZSUMTK1.
     MOVE      "CZSUMTK1"   TO   AB-FILE.
     MOVE      SUM-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CZSUMWWF.
     MOVE      "CZSUMWWF"   TO   AB-FILE.
     MOVE      SMW-STATUS   TO   AB-STS.
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
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     INPUT     CZSUMTK1.
     OPEN     OUTPUT    CZSUMWWF.
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
     ACCEPT   SYS-TIME          FROM   TIME.
*検品結果集計ファイル読込
     PERFORM CZSUMTK1-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　対象データ　　なし。＃＃"  UPON CONS
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    検品結果集計ファイル読込
****************************************************************
 CZSUMTK1-READ-SEC           SECTION.
*
     MOVE    "CZSUMTK1-READ-SEC"   TO   S-NAME.
*
 CZSUMTK1-READ-01.
     READ     CZSUMTK1  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   CZSUMTK1-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   CZSUMTK1-READ-CNT.
*条件判定
     IF       SUM-F17 = "1"
              INITIALIZE               SMW-REC
              MOVE   SUM-REC      TO   SMW-REC
              WRITE                    SMW-REC
              ADD        1        TO   CZSUMTK1-ERR-CNT
     ELSE
              GO                  TO   CZSUMTK1-READ-01
     END-IF.
*
 CZSUMTK1-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-020.
     PERFORM  CZSUMTK1-READ-SEC.
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
*件数表示
*  ＝検品結果集計ファイル読込件数
     DISPLAY "CZSUMTK1 READ-CNT  = " CZSUMTK1-READ-CNT UPON CONS.
     DISPLAY "OUTPUT   ERR-CNT   = " CZSUMTK1-ERR-CNT UPON CONS.
*
     CLOSE     CZSUMTK1
               CZSUMWWF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
