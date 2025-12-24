****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｊ本田オンラインシステム　　　　　*
*    業務名　　　　　　　：　Ｊ本田オンラインシステム　　　　　*
*    モジュール名　　　　：　入荷予定送信データ作成            *
*    作成日／更新日　　　：　2005/05/25                        *
*    作成者／更新者　　　：　C FUJIWARA                        *
*    処理概要　　　　　　：　入荷予定ワークを読み，Ｊ本田仕様  *
*                            の送信データを作成する。          *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7107B.
 AUTHOR.                C FUJIWARA.
 DATE-WRITTEN.          05/05/25.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*Ｊ本田入荷予定データ
     SELECT   HDNYKYSF  ASSIGN    TO        DA-01-S-HDNYKYSF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    HNS-STATUS.
*入荷予定ワーク
     SELECT   HDHEADF   ASSIGN    TO        DA-01-VI-HDHEADL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HNY-F01   HNY-F02
                                            HNY-F03   HNY-F04
                                            HNY-F05   HNY-F06
                        FILE      STATUS    HNY-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    Ｊ本田入荷予定データ
******************************************************************
 FD  HDNYKYSF           LABEL RECORD   IS   STANDARD.
 01  HNS-REC.
     03  HNS-FIL             PIC   X(128).
     03  HNS-REC-R      REDEFINES  HNS-FIL.
         05  HNS-F01         PIC   X(02).
         05  HNS-F02         PIC   X(02).
         05  HNS-F03         PIC   9(05).
         05  HNS-F04         PIC   X(119).
*
******************************************************************
*    入荷予定ワーク
******************************************************************
 FD  HDHEADF            LABEL RECORD   IS   STANDARD.
     COPY     HDHEADF   OF        XFDLIB
              JOINING   HNY       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  HNS-CNT             PIC  9(08)     VALUE  ZERO.
     03  SEQ-CNT             PIC  9(08)     VALUE  ZERO.
*    ﾌﾗｸﾞ
 01  WK-INV-FLG.
     03  END-FLG             PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE             PIC  9(06).
     03  SYS-DATEW            PIC  9(08).
*システム時間の編集
     03  SYS-TIME             PIC  9(08).
     03  FILLER            REDEFINES      SYS-TIME.
         05  SYS-TIMEW        PIC  9(06).
         05  SYS-MS           PIC  9(02).
 01  WK-ST.
     03  HNS-STATUS        PIC  X(02).
     03  HNY-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7107B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7107B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7107B".
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
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUDT.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HDNYKYSF.
     MOVE      "HDNYKYSF "   TO   AB-FILE.
     MOVE      HNS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HDHEADF.
     MOVE      "HDHEADF "   TO   AB-FILE.
     MOVE      HNY-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     HDHEADF.
     OPEN     OUTPUT    HDNYKYSF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO                TO   WK-CNT.
     MOVE     ZERO                TO   SEQ-CNT.
     MOVE     SPACE               TO   WK-INV-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT   SYS-DATE          FROM   DATE.
     ACCEPT   SYS-TIME          FROM   TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE    LINK-OUT-YMD8    TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*
*    入荷予定ワーク　ＲＥＡＤ
     PERFORM HDHEADF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　入荷予定ワーク　　　ＲＥＡＤ                        *
****************************************************************
 HDHEADF-READ-SEC    SECTION.
     MOVE     "HDHEADF-READ-SEC"  TO   S-NAME.
*
     READ    HDHEADF
         AT  END
             MOVE     "END"       TO   END-FLG
         NOT AT END
             ADD       1          TO   READ-CNT
     END-READ.
*
 HDHEADF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     MOVE     SPACE               TO   HNS-REC.
*
     ADD      1                   TO   SEQ-CNT.
     MOVE     HNY-F07             TO   HNS-REC.
     MOVE     SEQ-CNT             TO   HNS-F03.
*
     WRITE    HNS-REC.
     ADD      1                   TO   HNS-CNT.
*
*    入荷予定ワーク　ＲＥＡＤ
     PERFORM HDHEADF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "入荷予定Ｗ READ  CNT = " READ-CNT  UPON CONS.
     DISPLAY "入荷予定Ｄ WRITE CNT = " HNS-CNT   UPON CONS.
*
     CLOSE     HDNYKYSF  HDHEADF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
