****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　トークオンライン　　　　　　　　　*
*　　モジュール名　　　　：　支払合計ファイル作成　　　　　　　*
*　　作成日／更新日　　　：　95/07/25                          *
*　　作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*　　処理概要　　　　　　：　ＣＶＣＳ集信ファイルより　　　　　*
*　　　　　　　　　　　　：　ＣＶＣＳ送信用ファイルを作成する　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSI0160B.
 AUTHOR.                TOMIZAWA.
 DATE-WRITTEN.          95/07/25.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 集信データ >>--*
     SELECT   CVCSF     ASSIGN         DA-01-S-CVCSF
                        ORGANIZATION   SEQUENTIAL
                        STATUS    CVCS-ST.
*----<< 支払合計ファイル >>--*
     SELECT   HSHIGKF   ASSIGN         DA-01-S-HSHIGKF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SHI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 集信データ >>--*
 FD  CVCSF              LABEL     RECORD   IS   STANDARD.
 01  ONL-REC.
     03  ONL1-REC.
         05  ONL1-A1         PIC  X(03).
         05  ONL1-F90        PIC  X(253).
     03  ONL2-REC            REDEFINES  ONL1-REC.
         05  ONL2-C1         PIC  X(03).
         05  ONL2-C2         PIC  X(04).
         05  ONL2-C3         PIC  9(02).
         05  FILLER          PIC  X(19).
         05  ONL2-000.
             07  ONL2-C01    PIC  X(32)     OCCURS   7.
         05  ONL2-001        REDEFINES      ONL2-000.
             07  ONL2-C02    OCCURS    7.
                  09  ONL2-C4     PIC  9(03).
                  09  ONL2-C5     PIC  9(06).
                  09  ONL2-C6     PIC  9(09).
                  09  ONL2-C7     PIC  X(01).
                  09  ONL2-C8     PIC  9(10).
                  09  ONL2-C9     PIC  9(01).
                  09  FILLER      PIC  X(02).
         05  FILLER          PIC  X(04).
*----<< 支払合計ファイル >>--*
 FD  HSHIGKF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       20   RECORDS.
     COPY        SITGKFB     OF   XFDLIB
                 JOINING     SHI     PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  INP-X1         PIC  X(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  FH-CNT         PIC  9(06).
     03  MH-CNT         PIC  9(06).
     03  MS-CNT         PIC  9(06).
     03  GK-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
 01  INDEXES.
     03  I              PIC  9(03).
     03  J              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  CVCS-ST            PIC  X(02).
 01  SHI-ST             PIC  X(02).
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
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 集信データ >>--*
 CVCSK-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CVCSF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0160B CVCSF    ERROR " CVCS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 支払合計ファイル >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0160B HSHIGKF ERROR " SHI-ST " "
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
     DISPLAY  "*** SSI0160B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     CVCSF.
     OPEN     OUTPUT    HSHIGKF.
*ワークエリア　クリア
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     SPACE     TO   SHI-REC.
     INITIALIZE              SHI-REC.
*
     READ     CVCSF
        AT    END
              MOVE      1    TO   END-FLG
        NOT AT END
              ADD       1    TO   IN-CNT
     END-READ.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*
*****DISPLAY  ONL1-A1  UPON CONS.
     IF       ONL1-A1   NOT =     "58C"
              GO TO               MAIN-01
     END-IF.
*
     PERFORM       VARYING   I    FROM 1    BY   1
                   UNTIL     I    >    7
                   OR        ONL2-C01 (I)   =    SPACE
*
                   PERFORM   EDIT-SEC
     END-PERFORM.
*
 MAIN-01.
*
     READ     CVCSF
        AT    END
              MOVE      1    TO   END-FLG
        NOT AT END
              ADD       1    TO   IN-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    CVCSF.
     CLOSE    HSHIGKF.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI0160B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
**   ACCEPT   INP-X1         FROM CONS.
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　編集処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE     SPACE          TO   SHI-REC.
     INITIALIZE                   SHI-REC.
     MOVE     ZERO           TO   SHI-F32.
     MOVE     ONL2-C2        TO   SHI-F32.
     MOVE     "00000"        TO   SHI-F19.
     MOVE     ONL2-C4  (I)   TO   SHI-F19(3:3).
     MOVE     ONL2-C5  (I)   TO   SHI-F20.
     MOVE     ONL2-C6  (I)   TO   SHI-F21.
     IF       ONL2-C7  (I)   =     1
              COMPUTE  SHI-F22        =
                       ONL2-C8  (I)   *    -1
     ELSE
              MOVE     ONL2-C8  (I)   TO    SHI-F22
     END-IF.
*
     MOVE     ONL2-C9(I)     TO   SHI-F24.
*
     IF       ONL2-C9(I)     =    ZERO
        IF    ONL2-C7  (I)   =     1
              COMPUTE  SHI-F26        =
                       ONL2-C8  (I)   *    -1
        ELSE
              MOVE     ONL2-C8  (I)   TO    SHI-F26
        END-IF
     ELSE
        IF    ONL2-C7  (I)   =     1
              COMPUTE  SHI-F30        =
                       ONL2-C8  (I)   *    -1
        ELSE
              MOVE     ONL2-C8  (I)   TO    SHI-F30
        END-IF
     END-IF.
*
     EVALUATE          ONL2-C9 (I)
        WHEN  1
              MOVE     "ｼﾊﾗｲﾅｼ"       TO  SHI-F14
        WHEN  2
              MOVE     "ﾐｾﾁｶﾞｲ"       TO  SHI-F14
        WHEN  3
              MOVE     "ｷﾝｶﾞｸﾁｶﾞｲ"    TO  SHI-F14
        WHEN  4
              MOVE     "ﾐｾ ｷﾝｶﾞｸﾁｶﾞｲ" TO  SHI-F14
        WHEN  5
              MOVE     "ﾆｼﾞｭｳﾃﾞﾝﾋﾟｮｳ" TO  SHI-F14
     END-EVALUATE.
*
     WRITE    SHI-REC.
     ADD      1              TO   OUT-CNT.
*
 EDIT-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
