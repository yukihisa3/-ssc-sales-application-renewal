****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　殿　　　　　　*
*    業務名　　　　　　　：　出荷検品業務                      *
*    モジュール名　　　　：　商品名称マスタ送信Ｆ作成          *
*    作成日／更新日　　　：　2000/10/06                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品ＭとＢＫ商品Ｍを突合せ、送信用*
*                        ：　商品Ｍを作成する。　　　　　　　　*
*                            （全件用）                        *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKE0051B.
 AUTHOR.                T.T.
 DATE-WRITTEN.          00/10/06.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         YA        IS   YA
         YB-21     IS   YB-21
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 商品名称マスタ >>--*
     SELECT   HMEIMS    ASSIGN         DA-01-S-HMEIMS
                        ORGANIZATION   SEQUENTIAL
                        STATUS         MEI-ST.
*----<< 送信用商品名称マスタ >>--*
     SELECT   KNPSYKF   ASSIGN         DA-01-S-KNPSYKF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SYK-ST.
*----<< 送信用件数Ｆ >>--*
     SELECT   KNPSYOF   ASSIGN         DA-01-S-KNPSYOF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SYO-ST.
*----<< プリント Ｆ >>--*
     SELECT      PRINTF      ASSIGN    TO        LP-04-PRTF.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 商品名称マスタ >>--*
 FD  HMEIMS             LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  1   RECORDS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*----<< 送信用商品名称マスタ >>--*
 FD  KNPSYOF            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  1   RECORDS.
     COPY     KNPSYOF   OF        XFDLIB
              JOINING   SYO       PREFIX.
*----<< 送信件数ファイル >>--*
 FD  KNPSYKF            BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  SYK-REC.
     03  SYK-F01             PIC  9(08).
     03  SYK-F02             PIC  X(02).
*----<< プリント　ファイル >>---*
 FD  PRINTF.
 01  PRINT-REC                    PIC       X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  MEI-ST             PIC  X(02).
 01  SYO-ST             PIC  X(02).
 01  SYK-ST             PIC  X(02).
*----<< ﾌﾟﾛｸﾞﾗﾑID  >>--*
 01  PG-ID.
     03  ID-PG          PIC  X(08)     VALUE  "SKE0051B".
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
*----<< READ KEY >>--*
 01  FLG-AREA.
     03  END-FLG        PIC  X(03)    VALUE  SPACE.
 01  MEB-KEY.
     03  MEB-SYOCD      PIC  X(16)    VALUE  SPACE.
*----<< 件数ｶｳﾝﾄ >>--*
 01  WT-CNT             PIC  9(07)    VALUE   ZERO.
*----<< ﾃﾞｰﾀ区分 >>--*
 01  WK-KBN             PIC  X(01)    VALUE  SPACE.
*    名称編集１
 01  WK-HEN1.
     03  WK-HEN1-1          PIC   X(01).
     03  WK-HEN1-2          PIC   N(15).
     03  WK-HEN1-3          PIC   X(01).
*    入数編集
 01  WK-IRISU               PIC   9(04)V9(02)  VALUE  ZERO.
 01  WK-IRISU-R             REDEFINES   WK-IRISU.
     03  WK-IRISU-1         PIC   9(04).
     03  WK-IRISU-2         PIC   9(02).
 01  WK-IRISU-H.
     03  WK-IRISU-H1        PIC   9(04).
     03  WK-IRISU-H2        PIC   X(01).
     03  WK-IRISU-H3        PIC   9(02).
*    名称編集２
 01  WK-HEN-DATE.
     03  WK-HEN-DATE1       PIC   X(04).
     03  FILLER             PIC   X(01)  VALUE  "/".
     03  WK-HEN-DATE2       PIC   X(02).
     03  FILLER             PIC   X(01)  VALUE  "/".
     03  WK-HEN-DATE3       PIC   X(02).
*    日付変換１
 01  WK-HENKAN              PIC   9(08)  VALUE  ZERO.
 01  WK-TOUROKU.
     03  WK-TOUROKU1        PIC   9(04).
     03  WK-TOUROKU2        PIC   9(02).
     03  WK-TOUROKU3        PIC   9(02).
*    日付変換２
 01  WK-KOUSIN.
     03  WK-KOUSIN1         PIC   9(04).
     03  WK-KOUSIN2         PIC   9(02).
     03  WK-KOUSIN3         PIC   9(02).
*商品名称マスタ退避ワーク
     COPY   HMEIMS  OF XFDLIB  JOINING   WK  AS   PREFIX.
*    見出し行１
 01  MIDASHI1.
     03  FILLER                   PIC       X(38)  VALUE SPACE.
     03  FILLER                   PIC       N(17)  VALUE
       NC"【　出荷検品店舗振分件数リスト　】"
       CHARACTER   TYPE   IS   YB-21.
     03  FILLER                   PIC       X(18)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "DATE:".
     03  YY                       PIC       99.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  MM                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  DD                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "PAGE:".
     03  PEIJI                    PIC       ZZZ9.
*    見出し行２
 01  MIDASHI2           CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(50)  VALUE SPACE.
     03  FILLER                   PIC       N(19)  VALUE
       NC"商品名称マスタ送信Ｆ　データ件数　＝　".
     03  KENSU                    PIC       ZZ,ZZZ,ZZ9.
*    線１
 01  SEN1               CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                   PIC       N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                   PIC       N(18)  VALUE
         NC"──────────────────".
*    線２
 01  SEN2.
     03  FILLER                   PIC       X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                   PIC       X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                   PIC       X(36)  VALUE
         "------------------------------------".
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 商品名称マスタ >>--*
 MEI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HMEIMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " HMEIMS ERROR " MEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 送信用商品名称マスタ >>--*
 SYO-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KNPSYOF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " KNPSYOF ERROR " SYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 送信用件数ファイル >>--*
 SYK-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KNPSYKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " KNPSYKF ERROR " SYK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*開始メッセージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
******
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG = "END".
     PERFORM  300-END-RTN.
******
*終了メッセージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     STOP RUN.
*
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     INPUT     HMEIMS.
     OPEN     OUTPUT    KNPSYOF.
     OPEN     OUTPUT    KNPSYKF.
*****OPEN     OUTPUT    PRINTF.
*ファイル初期ＲＥＡＤ（キーのセット）
     PERFORM  900-MEI-READ.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*商品名称マスタセット
     MOVE      MEI-REC    TO    WK-REC.
*項目セット
     PERFORM   210-HENSYU-RTN.
*商品名称マスタ読込み
      PERFORM  900-MEI-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*件数プリント
*****PERFORM   LISTWT-SEC.
*件数Ｆ出力
     MOVE     SPACE               TO   SYK-REC.
     INITIALIZE                        SYK-REC.
*
     MOVE      WT-CNT             TO   SYK-F01.
     MOVE      X"0D0A"            TO   SYK-F02.
*
     WRITE     SYK-REC.
*ファイルのクローズ
*****CLOSE    HMEIMS  KNPSYOF  KNPSYKF  PRINTF.
     CLOSE    HMEIMS  KNPSYOF  KNPSYKF.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     送信用商品名称マスタ                         *
*--------------------------------------------------------------*
 210-HENSYU-RTN         SECTION.
*送信用商品名称マスタ初期化
     MOVE     SPACE               TO   SYO-REC.
     INITIALIZE                        SYO-REC.
*データ区分
     MOVE     "2"                 TO   SYO-F01.
*商品ＣＤ
     MOVE     WK-F01              TO   SYO-F02.
*商品名称１
     MOVE     WK-F021             TO   WK-HEN1-2.
     MOVE     X"28"               TO   WK-HEN1-1.
     MOVE     X"29"               TO   WK-HEN1-3.
     MOVE     WK-HEN1             TO   SYO-F03.
*商品名称２
     MOVE     WK-F022             TO   WK-HEN1-2.
     MOVE     X"28"               TO   WK-HEN1-1.
     MOVE     X"29"               TO   WK-HEN1-3.
     MOVE     WK-HEN1             TO   SYO-F04.
*商品名（カナ）１
     MOVE     WK-F031             TO   SYO-F05.
*商品名（カナ）２
     MOVE     WK-F032             TO   SYO-F06.
*ＪＡＮＣＤ
     MOVE     WK-F06              TO   SYO-F07.
*入数
     MOVE     WK-F07              TO   WK-IRISU.
     MOVE     WK-IRISU-1          TO   WK-IRISU-H1.
     MOVE     "."                 TO   WK-IRISU-H2.
     MOVE     WK-IRISU-2          TO   WK-IRISU-H3.
     MOVE     WK-IRISU-H          TO   SYO-F08.
*自動発注区分
     MOVE     WK-F92              TO   SYO-F09.
*商品名称区分
     MOVE     WK-F93              TO   SYO-F10.
*ユポラベル区分
     MOVE     WK-F94              TO   SYO-F11.
*廃盤区分
     MOVE     WK-F08              TO   SYO-F12.
*登録日付
     MOVE     WK-F98              TO   WK-HENKAN.
     MOVE     WK-HENKAN           TO   WK-TOUROKU.
     MOVE     WK-TOUROKU1         TO   WK-HEN-DATE1.
     MOVE     WK-TOUROKU2         TO   WK-HEN-DATE2.
     MOVE     WK-TOUROKU3         TO   WK-HEN-DATE3.
     MOVE     WK-HEN-DATE         TO   SYO-F13.
*更新日付
     MOVE     WK-F99              TO   WK-HENKAN.
     MOVE     WK-HENKAN           TO   WK-KOUSIN.
     MOVE     WK-KOUSIN1          TO   WK-HEN-DATE1.
     MOVE     WK-KOUSIN2          TO   WK-HEN-DATE2.
     MOVE     WK-KOUSIN3          TO   WK-HEN-DATE3.
     MOVE     WK-HEN-DATE         TO   SYO-F14.
*改行コード
     MOVE     X"0D0A"             TO   SYO-F15.
*送信用商品名称マスタ出力
     WRITE    SYO-REC.
*出力件数カウント
     ADD       1                TO      WT-CNT.
*
 210-HENSYU-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    商品名称マスタ　読込み                       *
*--------------------------------------------------------------*
 900-MEI-READ           SECTION.
*
     READ     HMEIMS
         AT END
              MOVE     "END"           TO   END-FLG
     END-READ.
*
 900-MEI-READ-EXIT.
     EXIT.
****************************************************************
*           リスト出力処理                          3.1.1      *
****************************************************************
 LISTWT-SEC   SECTION.
*
     MOVE      SYS-YY         TO        YY.
     MOVE      SYS-MM         TO        MM.
     MOVE      SYS-DD         TO        DD.
     MOVE      1              TO        PEIJI.
     MOVE      WT-CNT         TO        KENSU.
     WRITE     PRINT-REC      FROM      MIDASHI1 AFTER 2.
     WRITE     PRINT-REC      FROM      SEN1     AFTER 2.
     WRITE     PRINT-REC      FROM      MIDASHI2 AFTER 1.
     WRITE     PRINT-REC      FROM      SEN1     AFTER 1.
*
 LISTWT-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
