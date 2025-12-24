****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ　殿　　　　　　*
*    業務名　　　　　　　：　出荷検品業務                      *
*    モジュール名　　　　：　商品名称マスタ送信Ｆ作成          *
*    作成日／更新日　　　：　2008/03/24                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　件数カウントと件数ファイルの出力を*
*                        ：　行なう。                          *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKE5141B.
 AUTHOR.                T.T.
 DATE-WRITTEN.          08/03/24.
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
*----<< 送信用商品名称マスタ件数 >>--*
     SELECT   KNPSYKF   ASSIGN         DA-01-S-KNPSYKF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SYK-ST.
*----<< 送信用商品名称マスタ >>--*
     SELECT   KNPSYOF   ASSIGN         DA-01-S-KNPSYOF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SYO-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 送信用商品名称マスタ >>--*
 FD  KNPSYOF            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  24  RECORDS.
     COPY     KNPSYOF   OF        XFDLIB
              JOINING   SYO       PREFIX.
*----<< 送信用商品名称マスタ件数 >>--*
 FD  KNPSYKF            BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  SYK-REC.
     03  SYK-F01             PIC  9(08).
     03  SYK-F02             PIC  X(02).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SYO-ST             PIC  X(02).
 01  SYK-ST             PIC  X(02).
*----<< ﾌﾟﾛｸﾞﾗﾑID  >>--*
 01  PG-ID.
     03  ID-PG          PIC  X(08)     VALUE  "SKE5141B".
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
     03  HMEIMS-INV-FLG PIC  X(03)    VALUE  SPACE.
 01  MEB-KEY.
     03  MEB-SYOCD      PIC  X(16)    VALUE  SPACE.
*----<< 件数ｶｳﾝﾄ >>--*
 01  WT-CNT             PIC  9(07)    VALUE   ZERO.
 01  READ-CNT           PIC  9(07)    VALUE   ZERO.
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
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
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
*----<< 送信用商品名称マスタ件数 >>--*
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
     OPEN     INPUT     KNPSYOF.
     OPEN     OUTPUT    KNPSYKF.
*ファイル初期ＲＥＡＤ
     PERFORM  900-SHO-READ.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     ADD       1          TO     READ-CNT.
*
      PERFORM  900-SHO-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*件数Ｆ出力
     MOVE     SPACE               TO   SYK-REC.
     INITIALIZE                        SYK-REC.
*
     MOVE      READ-CNT           TO   SYK-F01.
     MOVE      X"0D0A"            TO   SYK-F02.
*
     WRITE     SYK-REC.
*ファイルのクローズ
     CLOSE     KNPSYOF  KNPSYKF.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    商品変換テーブル読込み                       *
*--------------------------------------------------------------*
 900-SHO-READ           SECTION.
*
     READ     KNPSYOF
              AT END
              MOVE     "END"           TO   END-FLG
     END-READ.
*
 900-SHO-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
