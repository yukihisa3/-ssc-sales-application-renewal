****************************************************************
*    顧客名　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　：　グッデイＥＤＩ
*    業務名　　　　　　：　出荷業務
*    モジュール名　　　：　出荷送信データ出力　　　　　　　　
*    作成日／更新日　　：　2014/06/16
*    作成者／更新者　　：　NAV
*    処理概要　　　　　：　出荷送信データより、送信形式の
*                          データに変換を行なう。　　　　　　
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY5764B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/06/16.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*グッデイ　出荷送信ファイル（ヘッダ）
     SELECT   GDSNDHD   ASSIGN    TO        DA-01-S-GDSNDHD
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SHD-STATUS.
*グッデイ　出荷送信ファイル（明細）
     SELECT   GDSNDMS   ASSIGN    TO        DA-01-S-GDSNDMS
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SMS-STATUS.
*グッデイ　出荷送信ファイル
     SELECT   GDSNDDT   ASSIGN    TO        DA-01-S-GDSNDDT
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SND-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*グッデイ　出荷送信ファイル（ヘッダ）
******************************************************************
 FD  GDSNDHD            BLOCK CONTAINS 1 RECORDS.
     COPY     GDSNDHD   OF        XFDLIB
              JOINING   SHD       PREFIX.
******************************************************************
*グッデイ　出荷送信ファイル（明細）
******************************************************************
 FD  GDSNDMS            BLOCK CONTAINS 1 RECORDS.
     COPY     GDSNDMS   OF        XFDLIB
              JOINING   SMS       PREFIX.
******************************************************************
*グッデイ　出荷送信ファイル
******************************************************************
 FD  GDSNDDT
                        BLOCK CONTAINS 1 RECORDS.
 01  SND-REC            PIC  X(256).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  HED-CNT             PIC  9(08)     VALUE  ZERO.
     03  MEI-CNT             PIC  9(08)     VALUE  ZERO.
     03  SND-CNT             PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  SND-STATUS        PIC  X(02).
     03  SHD-STATUS        PIC  X(02).
     03  SMS-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY5764B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY5764B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY5764B".
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
 01  PARA-KENSUU1           PIC   9(07).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION    USING  PARA-KENSUU1.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   GDSNDHD.
     MOVE      "GDSNDHD"    TO   AB-FILE.
     MOVE      SHD-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   GDSNDMS.
     MOVE      "GDSNDMS"    TO   AB-FILE.
     MOVE      SMS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   GDSNDDT.
     MOVE      "GDSNDDT"    TO   AB-FILE.
     MOVE      SND-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                               *
*****************************************************************
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
     OPEN     INPUT     GDSNDHD  GDSNDMS.
     OPEN     OUTPUT    GDSNDDT.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
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
*    グッデイ　　出荷送信ファイル（ヘッダ）読込
     PERFORM     GDSNDHD-READ-SEC.
     IF          END-FLG    =    "END"
                 GO  TO   INIT-EXIT
     END-IF.
*    送信ファイル出力
     MOVE     SPACE               TO   SND-REC.
     INITIALIZE                        SND-REC.
*ヘッダレコード出力
     MOVE     SHD-REC             TO   SND-REC.
     WRITE    SND-REC.
     ADD      1                   TO   SND-CNT.
*
*    グッデイ　　出荷送信ファイル（明細）読込
     PERFORM     GDSNDMS-READ-SEC.
     IF          END-FLG    =    "END"
                 GO  TO   INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　　出荷送信ファイル（ヘッダ）ＲＥＡＤ 　　　　　
****************************************************************
 GDSNDHD-READ-SEC    SECTION.
*
     READ     GDSNDHD
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  GDSNDHD-READ-EXIT
              NOT AT END
                  ADD       1       TO  HED-CNT
     END-READ.
*
     IF    HED-CNT(6:3)  =  "000" OR "500"
           DISPLAY "READ-CNT = " HED-CNT  UPON CONS
     END-IF.
*
 GDSNDHD-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    送信ファイル出力
     MOVE     SPACE               TO   SND-REC.
     INITIALIZE                        SND-REC.
*明細レコード出力
     MOVE     SMS-REC             TO   SND-REC.
     WRITE    SND-REC.
     ADD      1                   TO   SND-CNT.
 MAIN010.
*    グッデイ　　出荷送信ファイル（明細）読込
     PERFORM GDSNDMS-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　　出荷送信ファイル（明細）ＲＥＡＤ 　　　　　
****************************************************************
 GDSNDMS-READ-SEC    SECTION.
*
     READ     GDSNDMS
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  GDSNDMS-READ-EXIT
              NOT AT END
                  ADD       1       TO  MEI-CNT
     END-READ.
*
     IF    MEI-CNT(6:3)  =  "000" OR "500"
           DISPLAY "READ-CNT = " MEI-CNT  UPON CONS
     END-IF.
*
 GDSNDMS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      SND-CNT   TO      PARA-KENSUU1.
     DISPLAY NC"送信データ　ヘッダ　読込＝" HED-CNT  UPON CONS.
     DISPLAY NC"送信データ　明細　　読込＝" MEI-CNT  UPON CONS.
     DISPLAY NC"出荷送信ファイル　　出力＝" SND-CNT  UPON CONS.
*
     CLOSE    GDSNDHD  GDSNDMS  GDSNDDT.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
