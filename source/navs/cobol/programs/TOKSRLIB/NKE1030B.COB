****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　          *
*    モジュール名　　　　：　出荷検品結果データ件数カウント    *
*                              （カインズＴＣ１）              *
*    作成日／更新日　　　：　2019/06/25                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　検品システムから転送されたカインズ*
*                        ：　検品結果データの件数をカウントし、*
*                        ：　件数照会へ引き渡す。　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NKE1030B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/06/25.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<カインズ検品結果データ>>----*
     SELECT   RCVCZDXX  ASSIGN         DA-01-S-RCVCZDXX
                        ORGANIZATION   SEQUENTIAL
                        STATUS         RCV-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<カインズ検品結果データ>>----*
 FD  RCVCZDXX
*                       BLOCK CONTAINS 31 RECORDS.
*01  RCV-REC            PIC  X(132).
                        LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 31 RECORDS.
     COPY     RCVCZDXX  OF        XFDLIB
     JOINING  RCV  AS   PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  RCV-FLG        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  RCV-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  RCV-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "NKE1030B".
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*リンケージ
 LINKAGE                SECTION.
 01  PARA-OUT-KENSU          PIC 9(06).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-OUT-KENSU.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 RCVCZDXX-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVCZDXX.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1030B RCVCZDXX ERROR " RCV-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE1030B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*ファイルＯＰＥＮ
     OPEN     INPUT     RCVCZDXX.
*クリア
     INITIALIZE    WK-CNT  FLAGS.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*カインズ検品結果データカウント
     PERFORM RCV-RD-SEC  UNTIL  RCV-FLG = "END".
*
*カインズ検品結果データ件数セット
     MOVE    RCV-CNT             TO  PARA-OUT-KENSU.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルＣＬＯＳＥ
     CLOSE    RCVCZDXX.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE1030B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*       カインズ検品結果データ読込み                           *
*--------------------------------------------------------------*
 RCV-RD-SEC             SECTION.
     READ   RCVCZDXX  AT  END
            MOVE  "END"  TO  RCV-FLG
            NOT   AT  END
            ADD    1     TO  RCV-CNT
     END-READ.
 RCV-RD-EXIT.
     EXIT.
