****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷検品システム                  *
*    モジュール名　　　　：　本社側受信件数カウント            *
*    作成日／更新日　　　：　2001/04/16                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　出荷検品関連の指定倉庫ＣＤのファイ*
*                        ：　ル件数をカウントし、パラメタへ渡す*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKE0835B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/05/20.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<出荷情報ファイル>>----*
     SELECT   SYJOHOF   ASSIGN         DA-01-S-SYJOHOF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         JOH-ST.
*----<<封筒ラベル情報ファイル>>----*
     SELECT   SYLABHF   ASSIGN         DA-01-S-SYLABHF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         LAH-ST.
*----<<梱包ラベル情報ファイル>>----*
     SELECT   SYLABMF   ASSIGN         DA-01-S-SYLABMF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         LAM-ST.
*----<<出荷情報エラーファイル>>----*
     SELECT   RCVERRF   ASSIGN         DA-01-S-RCVERRF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         ERR-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<出荷情報ファイル>>----*
 FD  SYJOHOF
                        BLOCK CONTAINS 42 RECORDS.
 01  JOH-REC            PIC  X(96).
*----<<封筒ラベル情報ファイル>>----*
 FD  SYLABHF
                        BLOCK CONTAINS 88 RECORDS.
 01  LAH-REC            PIC  X(46).
*----<<梱包ラベル情報ファイル>>----*
 FD  SYLABMF
                        BLOCK CONTAINS 81 RECORDS.
 01  LAM-REC            PIC  X(50).
*----<<出荷情報エラーファイル>>----*
 FD  RCVERRF
                        BLOCK CONTAINS 42 RECORDS.
 01  ERR-REC            PIC  X(96).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  JOH-FLG        PIC  X(03)   VALUE SPACE.
     03  LAH-FLG        PIC  X(03)   VALUE SPACE.
     03  LAM-FLG        PIC  X(03)   VALUE SPACE.
     03  ERR-FLG        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  JOH-CNT        PIC  9(07).
     03  LAH-CNT        PIC  9(07).
     03  LAM-CNT        PIC  9(07).
     03  ERR-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  JOH-ST         PIC  X(02).
     03  LAH-ST         PIC  X(02).
     03  LAM-ST         PIC  X(02).
     03  ERR-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SKE0835B".
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
 01  PARA-SYJOHOF            PIC 9(06).
 01  PARA-SYLABHF            PIC 9(06).
 01  PARA-SYLABMF            PIC 9(06).
 01  PARA-RCVERRF            PIC 9(06).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-SYJOHOF
                                         PARA-SYLABHF
                                         PARA-SYLABMF
                                         PARA-RCVERRF.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 SYJOHOF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYJOHOF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0835B SYJOHOF ERROR " JOH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 SYLABHF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYLABHF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0835B SYLABHF ERROR " LAH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 SYLABMF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYLABMF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0835B SYLABHF ERROR " LAM-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 RCVERRF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVERRF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0835B RCVERRF ERROR " ERR-ST " "
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
     DISPLAY  "*** SKE0835B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*ファイルＯＰＥＮ
     OPEN     INPUT     SYJOHOF SYLABHF SYLABMF RCVERRF.
*クリア
     INITIALIZE    WK-CNT  FLAGS.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*出荷情報ファイルカウント
     PERFORM JOH-RD-SEC  UNTIL  JOH-FLG = "END".
*封筒ラベル情報ファイルカウント
     PERFORM LAH-RD-SEC  UNTIL  LAH-FLG = "END".
*梱包ラベル情報ファイルカウント
     PERFORM LAM-RD-SEC  UNTIL  LAM-FLG = "END".
*出荷情報エラーファイルカウント
     PERFORM ERR-RD-SEC  UNTIL  ERR-FLG = "END".
*出荷情報ファイル件数セット
     MOVE    JOH-CNT     TO  PARA-SYJOHOF.
*封筒ラベル情報ファイル件数セット
     MOVE    LAH-CNT     TO  PARA-SYLABHF.
*梱包ラベル情報ファイル件数セット
     MOVE    LAM-CNT     TO  PARA-SYLABMF.
*出荷情報エラーファイル件数セット
     MOVE    ERR-CNT     TO  PARA-RCVERRF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルＣＬＯＳＥ
     CLOSE    SYJOHOF SYLABHF SYLABMF RCVERRF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE0835B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             出荷情報ファイル読込み                           *
*--------------------------------------------------------------*
 JOH-RD-SEC             SECTION.
     READ   SYJOHOF  AT  END
            MOVE  "END"  TO  JOH-FLG
            NOT   AT  END
            ADD    1     TO  JOH-CNT
     END-READ.
 JOH-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           封筒ラベル情報ファイル読込み                       *
*--------------------------------------------------------------*
 LAH-RD-SEC             SECTION.
     READ   SYLABHF   AT  END
            MOVE  "END"  TO  LAH-FLG
            NOT   AT  END
            ADD    1     TO  LAH-CNT
     END-READ.
 LAH-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           梱包ラベル情報ファイル読込み                       *
*--------------------------------------------------------------*
 LAM-RD-SEC             SECTION.
     READ   SYLABMF  AT  END
            MOVE  "END"  TO  LAM-FLG
            NOT   AT  END
            ADD    1     TO  LAM-CNT
     END-READ.
 LAM-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           出荷情報エラーファイル読込み                       *
*--------------------------------------------------------------*
 ERR-RD-SEC             SECTION.
     READ   RCVERRF  AT  END
            MOVE  "END"  TO  ERR-FLG
            NOT   AT  END
            ADD    1     TO  ERR-CNT
     END-READ.
 ERR-RD-EXIT.
     EXIT.
