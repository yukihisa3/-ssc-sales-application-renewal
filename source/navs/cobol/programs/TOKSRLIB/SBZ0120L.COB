****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　部門間在庫移動機能構築　　　      *
*    モジュール名　　　　：　振替移動実績連携データ件数リスト  *
*    作成日／更新日　　　：　2018/01/18                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　振替移動実績連携データを読み、件　*
*                        ：　数をカウント後、件数リストを発行  *
*                        ：　する。　　　　　　　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBZ0120L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          18/01/18.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
     YA            IS        CHR-2
     YA-12         IS        CHR-12
     YA-22         IS        CHR-22
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<HG社内振替IFﾌｧｲﾙ>>----*
     SELECT   FURIACOS  ASSIGN         DA-01-S-FURIACOS
                        ORGANIZATION   SEQUENTIAL
                        STATUS         ACS-ST.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN  TO     LP-04-PRTF
                        FILE    STATUS PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<HG社内振替IFﾌｧｲﾙ>>----*
 FD  FURIACOS
                        BLOCK CONTAINS 1 RECORDS.
 01  ACS-REC            PIC  X(90).
*----<<プリントファイル>>----*
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ACS-FLG        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  ACS-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  ACS-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  PG-ID              PIC  X(08)     VALUE  "SBZ0120L".
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
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-00                  PIC  X(08).
     03  FILLER                  PIC  X(26)  VALUE  SPACE.
     03  FILLER                  PIC  N(15)  VALUE
         NC"＜振替移動実績連携件数リスト＞"
                                 CHARACTER  TYPE  IS  CHR-22.
     03  FILLER                  PIC  X(17)  VALUE  SPACE.
     03  HD1-01                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-04                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2.
     03  FILLER                  PIC  X(116) VALUE  SPACE.
     03  HD2-01                  PIC  99.
     03  FILLER                  PIC  X(01)  VALUE ":".
     03  HD2-02                  PIC  99.
     03  FILLER                  PIC  X(01)  VALUE ":".
     03  HD2-03                  PIC  99.
*
 01  SEN.
     03  FILLER                  PIC  X(136) VALUE  ALL "-".
*
 01  DT1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(50)  VALUE  SPACE.
     03  FILLER                  PIC  N(12)  VALUE
         NC"振替移動連携データ件数：".
     03  DT1-01                  PIC  Z,ZZZ,ZZ9.
*
 01  DT2                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(44)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
         NC"＜ＭＳＧ＞".
*
 01  DT3                         CHARACTER  TYPE  IS  CHR-12.
     03  FILLER                  PIC  X(46)  VALUE  SPACE.
     03  DT3-01                  PIC  N(30).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 ACSERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      FURIACOS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBZ0120L FURIACOS ERROR " ACS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBZ0120L PRTFILE ERROR " PRT-ST " "
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
     DISPLAY  "*** SBZ0120L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     FURIACOS.
     OPEN     OUTPUT    PRTFILE.
*クリア
     INITIALIZE    WK-CNT  FLAGS.
*ヘッダ行印字
     PERFORM       HEAD-WT-SEC.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*本社件数カウント
*    売上件数カウント
     PERFORM ACS-RD-SEC  UNTIL  ACS-FLG = "END".
*    帳票エリアセット
     MOVE     ACS-CNT             TO        DT1-01.
     WRITE    PRT-REC   FROM      DT1       AFTER     1.
*
     IF  ACS-CNT  >  ZERO
         WRITE  PRT-REC FROM      DT2       AFTER     3
         MOVE   SPACE             TO        DT3-01
         MOVE NC"振替移動実績計上対象データが作成されました。"
              TO                            DT3-01
         WRITE  PRT-REC FROM      DT3       AFTER     3
         MOVE   SPACE             TO        DT3-01
         MOVE NC"作成件数を情報ＳＹＳ部に報告し、取込処理を実施"
              TO                            DT3-01
         WRITE  PRT-REC FROM      DT3       AFTER     3
         MOVE   SPACE             TO        DT3-01
         MOVE NC"依頼を行って下さい。"
              TO                            DT3-01
         WRITE  PRT-REC FROM      DT3       AFTER     3
     ELSE
         WRITE  PRT-REC FROM      DT2       AFTER     3
         MOVE   SPACE             TO        DT3-01
         MOVE NC"本日は振替移動実績データの連携はありません。"
              TO                            DT3-01
         WRITE  PRT-REC FROM      DT3       AFTER     3
         MOVE   SPACE             TO        DT3-01
         MOVE NC"情報ＳＹＳ部様に送信データが無いことを連絡して"
              TO                            DT3-01
         WRITE  PRT-REC FROM      DT3       AFTER     3
         MOVE   SPACE             TO        DT3-01
         MOVE NC"下さい。"
              TO                            DT3-01
         WRITE  PRT-REC FROM      DT3       AFTER     3
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    FURIACOS  PRTFILE.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SBZ0120L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  売上計上データ読込み                        *
*--------------------------------------------------------------*
 ACS-RD-SEC             SECTION.
     READ   FURIACOS  AT  END
            MOVE  "END"  TO  ACS-FLG
            NOT   AT  END
            ADD    1     TO  ACS-CNT
     END-READ.
 ACS-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ヘッダ部出力処理                                 *
*--------------------------------------------------------------*
 HEAD-WT-SEC                  SECTION.
*項目設定
***  プログラムＩＤ
     MOVE     PG-ID               TO        HD1-00.
***  日付
     MOVE     "3"                 TO        LINK-IN-KBN.
     MOVE     SYS-DATE            TO        LINK-IN-YMD6.
     MOVE     ZERO                TO        LINK-IN-YMD8.
     MOVE     ZERO                TO        LINK-OUT-RET.
     MOVE     ZERO                TO        LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING        LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD(1:4)   TO        HD1-01.
     MOVE     LINK-OUT-YMD(5:2)   TO        HD1-02.
     MOVE     LINK-OUT-YMD(7:2)   TO        HD1-03.
***  システム時刻
     MOVE     SYS-HH              TO        HD2-01.
     MOVE     SYS-MN              TO        HD2-02.
     MOVE     SYS-SS              TO        HD2-03.
***  ページ_
     MOVE     1                   TO        HD1-04.
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  3.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  2.
***  改行
     MOVE     SPACE               TO        PRT-REC.
     WRITE    PRT-REC                       AFTER  2.
*
 HEAD-WT-EXIT.
     EXIT.
