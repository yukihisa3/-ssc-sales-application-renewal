****************************************************************
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　コーナン　ＥＤＩ　　　　　　      *
*　　モジュール名　　　　：　ＰＯＲ件数カウント　　　　　　　  *
*　　作成日／更新日　　　：　2021/01/08                        *
*　　作成者／更新者　　　：　NAV                               *
*　　処理概要　　　　　　：　コーナンＰＯＲ／ケースデータを読み*
*　　　　　　　　　　　　　　レコード件数をカウントしパラメタを*
*　　　　　　　　　　　　　　出力する。　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NJH1207B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/01/08.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< コーナンＰＯＲ／ケースデータ >>--*
     SELECT   KONANPOR  ASSIGN    TO   DA-01-S-KONANPOR
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KON-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< コーナンＰＯＲ／ケースデータ  >>--*
 FD  KONANPOR           LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       1    RECORDS.
 01  KON-REC.
   03  KONAN-F01.
       05  KONAN-F011   PIC  X(02).
       05  FILLER       PIC  X(101).
       05  KONAN-F012   PIC  9(01).
       05  FILLER       PIC  X(24).
*  03  KONAN-F02.
*      05  KONAN-F021   PIC  X(02).
*      05  FILLER       PIC  X(126).
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(07)     VALUE  ZERO.
     03  PD0-CNT        PIC  9(07)     VALUE  ZERO.
     03  PD1-CNT        PIC  9(07)     VALUE  ZERO.
     03  PP-CNT         PIC  9(07)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  KON-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  WK-SYS-DATE        PIC  9(08).
 01  WK-SYS-TIME        PIC  9(06).
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
 01  LINK-AREA2.
     03  LINK-IN-KBN    PIC  X(01).
     03  LINK-IN-YMD6   PIC  9(06).
     03  LINK-IN-YMD8   PIC  9(08).
     03  LINK-OUT-RET   PIC  X(01).
     03  LINK-OUT-YMD8  PIC  9(08).
 LINKAGE                SECTION.
 01  PARA-PD0-CNT       PIC  9(07).
 01  PARA-PD1-CNT       PIC  9(07).
 01  PARA-PP-CNT        PIC  9(07).
*
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-PD0-CNT
                                         PARA-PD1-CNT
                                         PARA-PP-CNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< コーナンＰＯＲ／ケースデータ >>--*
 KON-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE   KONANPOR.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NJH1207B KONANPOR ERROR " KON-ST " "
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
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =   "END".
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
*    システム日付８桁変換
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     MOVE     LINK-OUT-YMD8  TO   WK-SYS-DATE.
     MOVE     SYS-TIME(1:6)  TO   WK-SYS-TIME.
*ファイルＯＰＥＮ
     OPEN     INPUT     KONANPOR.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
*コーナン受信Ｆ読込
     PERFORM  KONANPOR-READ-SEC.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*発注明細／ラベル数をカウントする。
     IF  KONAN-F01  NOT =  SPACE
         EVALUATE  KONAN-F011
             WHEN  "PD"
                   IF    KONAN-F012 =  0
                         ADD    1     TO    PD0-CNT
                   ELSE
                         ADD    1     TO    PD1-CNT
                   END-IF
             WHEN  "PP"  ADD    1     TO    PP-CNT
         END-EVALUATE
     END-IF.
*
*    IF  KONAN-F021  NOT =  SPACE
*        EVALUATE  KONAN-F021
*            WHEN  "DD"  ADD    1     TO    PD0-CNT
*            WHEN  "DJ"  ADD    1     TO    PP-CNT
*        END-EVALUATE
*    END-IF.
*コーナン受信Ｆ読込
     PERFORM  KONANPOR-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    KONANPOR.
*読込カウントをパラメタにセット
     MOVE     PD0-CNT        TO    PARA-PD0-CNT.
     MOVE     PD1-CNT        TO    PARA-PD1-CNT.
     MOVE     PP-CNT         TO    PARA-PP-CNT.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NJH1207B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　コーナン受信Ｆ読込　　　　　　　　　　　　　　　　　*
****************************************************************
 KONANPOR-READ-SEC       SECTION.
*    コーナン受信Ｆ
     READ     KONANPOR  AT  END
              MOVE  "END"    TO   END-FLG
              GO             TO   KONANPOR-READ-EXIT
     END-READ.
*    読込みカウント
     ADD      1              TO   IN-CNT.
     IF       IN-CNT(5:3) = "000" OR "500"
              DISPLAY "IN-CNT = " IN-CNT UPON CONS
     END-IF.
*
 KONANPOR-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
