****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品システム                  *
*　　モジュール名　　　　：　検品送信結果ファイル作成　　　　　*
*　　作成日／更新日　　　：　00/11/15                          *
*　　作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*　　処理概要　　　　　　：　検品送信結果ファイルを作成する    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE0240B.
 AUTHOR.                N.KANEKO.
 DATE-WRITTEN.          00/11/15.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 検品送信結果パラメータＦ >>--*
     SELECT   SNDFINPF   ASSIGN         DA-01-S-SNDFINPF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SDP-ST.
*----<< 累積検品明細Ｆ >>--*
     SELECT   RUISYUF   ASSIGN         DA-01-VI-RUISYUL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY  RUI-F01   RUI-F06
                                       RUI-F07
                        FILE      STATUS    IS   RUI-ST.
*----<< 伝票データ >>--*
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY  DEN-F01   DEN-F02
                                       DEN-F04   DEN-F051
                                       DEN-F03
                        FILE      STATUS    IS   DEN-ST.
*----<< 検品送信結果Ｆ >>--*
     SELECT   SNDFINF   ASSIGN         DA-01-S-SNDFINF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SDF-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 検品送信結果パラメータＦ >>--*
 FD  SNDFINPF            LABEL RECORD   IS   STANDARD
     COPY        SNDFINPF     OF      XFDLIB
                 JOINING     SDP     PREFIX.
*----<< 累積検品明細Ｆ >>--*
 FD  RUISYUF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       1    RECORDS.
     COPY        RUISYUF     OF      XFDLIB
                 JOINING     RUI     PREFIX.
*----<< 売上伝票Ｆ >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 検品送信結果Ｆ >>--*
 FD  SNDFINF            LABEL RECORD   IS   STANDARD.
     COPY        SNDFINF     OF      XFDLIB
                 JOINING     SDF     PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  CHK-FLG        PIC  X(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
     03  ERR-CNT        PIC  9(06).
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  SDP-KEY.
     03  SDP-KEY-F01    PIC  9(08).
     03  SDP-KEY-F02    PIC  9(08).
 01  RUI-KEY.
     03  RUI-KEY-F01    PIC  9(08).
     03  RUI-KEY-F02    PIC  9(08).
 01  ERR-MSG            PIC  X(29)     VALUE
         "ｳﾘｱｹﾞﾃﾞﾝﾋﾟｮｳﾃﾞｰﾀ ｱﾝﾏｯﾁ KEY = ".
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SDP-ST             PIC  X(02).
 01  RUI-ST             PIC  X(02).
 01  DEN-ST             PIC  X(02).
 01  SDF-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
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
 LINKAGE                SECTION.
 01  PARA-INCNT         PIC  9(06).
 01  PARA-OUTCNT        PIC  9(06).
 01  PARA-ERRCNT        PIC  9(06).
*
****************************************************************
 PROCEDURE              DIVISION  USING     PARA-INCNT
                                            PARA-OUTCNT
                                            PARA-ERRCNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 検品送信結果パラメータＦ >>--*
 SDP-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SNDFINPF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKE0240B SNDFINPF    ERROR " SDP-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 累積検品明細Ｆ >>--*
 RUI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RUISYUF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKE0240B RUISYUF ERROR " RUI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 売上伝票Ｆ >>--*
 DEN-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKE0240B SNDFINF ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 検品送信結果Ｆ >>--*
 SDF-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SNDFINF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKE0240B SNDFINF ERROR " SDF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PROG-CNTL              SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =    1.
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE0240B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SNDFINPF.
     OPEN     INPUT     RUISYUF.
     OPEN     I-O       SHTDENF.
     OPEN     OUTPUT    SNDFINF.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*
     PERFORM  210-READ-SEC.
     PERFORM  220-READ-SEC.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*
     EVALUATE TRUE
*
         WHEN SDP-KEY   =    RUI-KEY
              PERFORM   230-WRITE-SEC
              PERFORM   220-READ-SEC
*
         WHEN SDP-KEY   >    RUI-KEY
              PERFORM   220-READ-SEC
*
         WHEN SDP-KEY   <    RUI-KEY
              PERFORM   210-READ-SEC
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    SNDFINPF.
     CLOSE    RUISYUF.
     CLOSE    SHTDENF.
     CLOSE    SNDFINF.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
     DISPLAY  "+++ ERROR      =" ERR-CNT " +++" UPON CONS.
     MOVE     IN-CNT         TO   PARA-INCNT.
     MOVE     OUT-CNT        TO   PARA-OUTCNT.
     MOVE     ERR-CNT        TO   PARA-ERRCNT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE0240B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　インプットデータ読む　　　　　　　　　　　　　　　　　*
****************************************************************
 210-READ-SEC           SECTION.
*
     READ     SNDFINPF
        AT    END
              MOVE      1    TO   END-FLG
              MOVE HIGH-VALUE     TO   SDP-KEY
        NOT AT END
              ADD       1    TO   IN-CNT
              MOVE      SDP-F01   TO   SDP-KEY-F01
              MOVE      SDP-F02   TO   SDP-KEY-F02
     END-READ.
*
 210-READ-EXIT.
     EXIT.
****************************************************************
*　　　　累積検品明細Ｆ読む　　　　　　　　　　　　　　　　　　*
****************************************************************
 220-READ-SEC           SECTION.

     READ     RUISYUF
        AT  END
              MOVE      1    TO   END-FLG
              MOVE HIGH-VALUE     TO   RUI-KEY
        NOT AT END
              MOVE      RUI-F01   TO   RUI-KEY-F01
              MOVE      RUI-F05   TO   RUI-KEY-F02
     END-READ.
*
 220-READ-EXIT.
     EXIT.
****************************************************************
*　　　　検品送信結果ファイル出力　　　　　　　　　　　　　　　*
****************************************************************
 230-WRITE-SEC          SECTION.
*送信データ出力
     MOVE     SPACE          TO   SDF-REC.
     INITIALIZE                   SDF-REC.
     MOVE     RUI-F01        TO   SDF-F01.
     MOVE     RUI-F06        TO   SDF-F02.
     MOVE     RUI-F07        TO   SDF-F03.
     MOVE     X"0D0A"        TO   SDF-F04.
     WRITE    SDF-REC.
     ADD      1              TO   OUT-CNT.
*売上伝票データ更新
     MOVE     RUI-F01        TO   DEN-F01.
     MOVE     RUI-F06        TO   DEN-F02.
     MOVE     RUI-F07        TO   DEN-F03.
     MOVE     ZERO           TO   DEN-F04.
     MOVE     40             TO   DEN-F051.
     READ     SHTDENF
        INVALID
              DISPLAY   ERR-MSG   RUI-F01 RUI-F07 RUI-F07
              UPON CONS
              ADD       1    TO   ERR-CNT
        NOT INVALID
              MOVE      1    TO   DEN-F27A
              REWRITE        DEN-REC
     END-READ.
*
 230-WRITE-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
