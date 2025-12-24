****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　コーナン受発注変更　　　　　　　　*
*    モジュール名　　　　：　送信データ作成　　　　　　　　　　*
*    作成日／更新日　　　：　04/07/27                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　取引先修正データより　　　　　　　*
*　　　　　　　　　　　　：　送信用ファイルを作成する          *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY1220B.
 AUTHOR.                Y.HATANO.
 DATE-WRITTEN.          04/07/27.
 DATE-COMPILED.
 SECURITY.              NONE.
*
 ENVIRONMENT            DIVISION.
*
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<<送信データHEADF>>-*
     SELECT   KOUHEDF   ASSIGN    TO        DA-01-S-KOUHEDF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   KHD-ST.
*----<< 取引先修正データ >>--*
     SELECT   KOUSYUF   ASSIGN     TO       DA-01-S-KOUSYUF
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KOU-ST
                        ORGANIZATION   IS   SEQUENTIAL.
*----<< 送信データファイル >>--*
     SELECT   SNDFILE     ASSIGN         DA-01-S-SNDFILE
                        ORGANIZATION   SEQUENTIAL
                        STATUS    CVCS-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 送信データHEADF >>-*
 FD  KOUHEDF            BLOCK CONTAINS 81   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     KOUHEDF   OF        XFDLIB
              JOINING   KHD       PREFIX.
*----<< 取引先修正データ >>--*
 FD  KOUSYUF            LABEL RECORD   IS   STANDARD.
     COPY     KOUSYUF   OF        XFDLIB
              JOINING   KOU       PREFIX.
*----<< 配信データ >>--*
 FD  SNDFILE              LABEL     RECORD   IS   STANDARD.
 01  SND-REC.
     03  SND0-REC                 PIC  X(256).
     03  SNDR1-REC  REDEFINES     SND0-REC.
       05  SND1-REC               OCCURS    4.
         07  SND1-A1                 PIC  9(06).
         07  SND1-A2                 PIC  9(02).
         07  SND1-A3                 PIC  9(01).
         07  SND1-A4                 PIC  9(01).
         07  SND1-A5                 PIC  9(05).
         07  SND1-A6                 PIC  9(01).
         07  SND1-A7                 PIC  9(05).
         07  SND1-A8                 PIC  9(08).
         07  SND1-A9                 PIC  9(06).
         07  SND1-A10                PIC  X(29).
     03  SNDR2-REC  REDEFINES     SND0-REC.
       05  SND2-REC               OCCURS    4.
         07  SND2-A1                 PIC  9(06).
         07  SND2-A2                 PIC  9(02).
         07  SND2-A3                 PIC  9(01).
         07  SND2-A4                 PIC  9(01).
         07  SND2-A5                 PIC  9(05).
         07  SND2-A6                 PIC  X(08).
         07  SND2-A7                 PIC  X(13).
         07  SND2-A8                 PIC  9(01).
         07  SND2-A9                 PIC  9(05).
         07  SND2-A10                PIC  9(05).
         07  SND2-A11                PIC  9(01).
         07  SND2-A12                PIC  X(08).
         07  SND2-A13                PIC  9(06).
         07  SND2-A14                PIC  9(02).
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01)    VALUE  ZERO.
     03  IDX            PIC  9(01)    VALUE  ZERO.
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
     03  REC-CNT        PIC  9(07).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  KHD-ST             PIC  X(02).
 01  KOU-ST             PIC  X(02).
 01  CVCS-ST            PIC  X(02).
*
*----<< ｼｭｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  KEI-KEN        PIC  9(05)     VALUE  ZERO.
     03  KEI-KIN        PIC S9(09)     VALUE  ZERO.
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-AREA.
     03  WK-TORICD      PIC  9(06).
     03  WK-ROOT        PIC  9(02).
     03  SYS-DATEW      PIC 9(08).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 送信データHEADF >>--*
 KOUHEDF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KOUHEDF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY1220B KOUHEDF ERROR " KHD-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 取引先修正データ >>--*
 KOUSYUF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KOUSYUF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY1220B KOUSYUF ERROR " KOU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 送信データ >>--*
 CVCSKHAI-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SNDFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY1220B SNDFILE    ERROR " CVCS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 GENERAL-PROCESS        SECTION.
*
     PERFORM  INIT-RTN.
     IF       END-FLG   =    0
              PERFORM       MAIN-RTN
                            UNTIL END-FLG = 1
     END-IF.
     PERFORM  END-RTN.
     STOP RUN.
 GENERAL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.

*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     INITIALIZE         GOKEI-AREA.
     INITIALIZE         WK-AREA.
*
     MOVE     99        TO        WK-ROOT.
*

     ACCEPT   SYS-DATE       FROM DATE.
*システム日付編集
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

     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY1220B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     KOUHEDF.
     OPEN     INPUT     KOUSYUF.
     OPEN     OUTPUT    SNDFILE.

     READ     KOUSYUF
           AT END
              MOVE     1          TO  END-FLG
           NOT AT END
              ADD      1          TO  IN-CNT
     END-READ.

*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*
*
*    ルートコードブレイク処理
     IF  KOU-F04  NOT   =   WK-ROOT
         MOVE   KOU-F04         TO   WK-ROOT
*
         IF   IDX       >    ZERO
              WRITE     SND-REC
              ADD       1    TO   OUT-CNT
              MOVE      SPACE     TO   SND-REC
              INITIALIZE               SND-REC
         END-IF

         PERFORM   KHD-READ
*
         MOVE      1         TO   IDX
*
*        HEDレコード出力
         MOVE     SPACE          TO   SND1-REC(IDX)
         INITIALIZE                   SND1-REC(IDX)
         MOVE     KHD-F01        TO   SND1-A1 (IDX)
         MOVE     KHD-F02        TO   SND1-A2 (IDX)
         MOVE     2              TO   SND1-A3 (IDX)
         MOVE     ZERO           TO   SND1-A4 (IDX)
         MOVE     1              TO   SND1-A5 (IDX)
         MOVE     1              TO   REC-CNT
         MOVE     1              TO   SND1-A6 (IDX)
         MOVE     KHD-F03        TO   SND1-A7 (IDX)
         MOVE     SYS-DATEW      TO   SND1-A8 (IDX)
         MOVE     SYS-TIME(1:6)  TO   SND1-A9 (IDX)
*
     END-IF.
*
     ADD      1              TO   IDX.
*    明細レコード出力
     MOVE     SPACE          TO   SND2-REC(IDX).
     INITIALIZE                   SND2-REC(IDX).
     MOVE     KOU-F03        TO   SND2-A1 (IDX).
     MOVE     KOU-F04        TO   SND2-A2 (IDX).
     MOVE     2              TO   SND2-A3 (IDX).
     ADD      1              TO   REC-CNT.
     MOVE     1              TO   SND2-A4 (IDX).
     MOVE     REC-CNT        TO   SND2-A5 (IDX).
     MOVE     KOU-F08        TO   SND2-A6 (IDX).
     MOVE     KOU-F09        TO   SND2-A7 (IDX).
     MOVE     ZERO           TO   SND2-A8 (IDX).
     MOVE     KOU-F11        TO   SND2-A9 (IDX).
     MOVE     KOU-F12        TO   SND2-A10 (IDX).
     MOVE     ZERO           TO   SND2-A11 (IDX).
     MOVE     KOU-F14        TO   SND2-A12 (IDX).
     MOVE     ZERO           TO   SND2-A13 (IDX).
     MOVE     ZERO           TO   SND2-A14 (IDX).
*
     IF       IDX       =    4
              WRITE     SND-REC
              ADD       1         TO   OUT-CNT
              MOVE      ZERO      TO   IDX
              MOVE      SPACE     TO   SND-REC
              INITIALIZE               SND-REC
     END-IF.

     READ     KOUSYUF
           AT END
              MOVE      1         TO  END-FLG
           NOT AT END
              ADD       1         TO  IN-CNT
     END-READ.
*
 MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　終了処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     IF   IDX       >    ZERO
          WRITE     SND-REC
          ADD       1    TO   OUT-CNT
     END-IF.
*
     CLOSE    KOUHEDF.
     CLOSE    KOUSYUF.
     CLOSE    SNDFILE.
*
     DISPLAY "+++ ｺﾞｳｹｲﾃﾞｰﾀ INPUT =" IN-CNT  " +++" UPON CONS.
     DISPLAY "+++ ｾｲｷｭｳﾃﾞｰﾀ OUTPUT=" OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY1220B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS        UPON CONS.
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　送信ＨＥＤデータＲＥＡＤ　　　　　　　　　　　　　　　*
****************************************************************
 KHD-READ               SECTION.
     READ     KOUHEDF   AT   END
              GO   TO   KHD-READ-EXIT
     END-READ.
*
 KHD-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
