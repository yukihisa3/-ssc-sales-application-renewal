# SKE0052B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKE0052B.COB`

## ソースコード

```cobol
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
 PROGRAM-ID.            SKE0052B.
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
*----<< 商品変換テーブル >>--*
     SELECT   HSHOTBL   ASSIGN         DA-01-VI-SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS  MODE   SEQUENTIAL
                        RECORD  KEY    SHO-F01
                                       SHO-F02
                        STATUS         SHO-ST.
*----<< 商品名称マスタ >>--*
     SELECT   HMEIMS    ASSIGN         DA-01-VI-MEIMS1
                        ORGANIZATION   INDEXED
                        ACCESS  MODE   RANDOM
                        RECORD  KEY    MEI-F011
                                       MEI-F0121
                                       MEI-F0122
                                       MEI-F0123
                        STATUS         MEI-ST.
*----<< 送信用商品名称マスタ >>--*
     SELECT   KNPSYKF   ASSIGN         DA-01-S-KNPSYKF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SYK-ST.
*----<< 送信用件数Ｆ >>--*
     SELECT   KNPSYOF   ASSIGN         DA-01-S-KNPSYOF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SYO-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 商品変換テーブル >>--*
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*----<< 商品名称マスタ >>--*
 FD  HMEIMS             LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  1   RECORDS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*----<< 送信用商品名称マスタ >>--*
 FD  KNPSYOF            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  24  RECORDS.
     COPY     KNPSYOF   OF        XFDLIB
              JOINING   SYO       PREFIX.
*----<< 送信件数ファイル >>--*
 FD  KNPSYKF            BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  SYK-REC.
     03  SYK-F01             PIC  9(08).
     03  SYK-F02             PIC  X(02).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHO-ST             PIC  X(02).
 01  MEI-ST             PIC  X(02).
 01  SYO-ST             PIC  X(02).
 01  SYK-ST             PIC  X(02).
*----<< ﾌﾟﾛｸﾞﾗﾑID  >>--*
 01  PG-ID.
     03  ID-PG          PIC  X(08)     VALUE  "SKE0052B".
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
*商品名称マスタ退避ワーク
     COPY   HMEIMS  OF XFDLIB  JOINING   WK  AS   PREFIX.
 LINKAGE               SECTION.
 01  PARA-TORICD       PIC  9(08).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-TORICD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 商品変換テーブル >>--*
 SHO-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " HSHOTBL ERROR " SHO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
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
     OPEN     INPUT     HSHOTBL.
     OPEN     INPUT     HMEIMS.
     OPEN     OUTPUT    KNPSYOF.
     OPEN     OUTPUT    KNPSYKF.
*商品変換テーブルスタート
     MOVE     PARA-TORICD  TO  SHO-F01.
     MOVE     SPACE        TO  SHO-F02.
     START  HSHOTBL  KEY  IS  >=  SHO-F01 SHO-F02
            INVALID
            DISPLAY "## ﾀｲｼｮｳﾃﾞｰﾀﾅｼ ##" UPON CONS
            MOVE    "END"      TO       END-FLG
            GO                 TO       100-INIT-RTN-EXIT
     END-START.
*ファイル初期ＲＥＡＤ（キーのセット）
     PERFORM  900-SHO-READ.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*商品名称マスタチェック
     PERFORM   900-MEI-READ.
     IF        HMEIMS-INV-FLG = "INV"
               GO         TO    MAIN010
     END-IF.
*商品名称マスタセット
     MOVE      MEI-REC    TO    WK-REC.
*項目セット
     PERFORM   210-HENSYU-RTN.
 MAIN010.
*商品変換テーブル読込み
      PERFORM  900-SHO-READ.
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
     CLOSE    HSHOTBL  HMEIMS  KNPSYOF  KNPSYKF.
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
     MOVE     SHO-F02             TO   SYO-F07.
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
     MOVE     SHO-F031    TO   MEI-F011.
     MOVE     SHO-F0321   TO   MEI-F0121.
     MOVE     SHO-F0322   TO   MEI-F0122.
     MOVE     SHO-F0323   TO   MEI-F0123.
*****IF       READ-CNT  =  100  OR  200  OR  300
*****         DISPLAY "SHO-F031  = " SHO-F031  UPON CONS
*****         DISPLAY "SHO-F0321 = " SHO-F0321 UPON CONS
*****         DISPLAY "SHO-F0322 = " SHO-F0322 UPON CONS
*****         DISPLAY "SHO-F0323 = " SHO-F0323 UPON CONS
*****         DISPLAY "MEI-F011  = " MEI-F011  UPON CONS
*****         DISPLAY "MEI-F0121 = " MEI-F0121 UPON CONS
*****         DISPLAY "MEI-F0122 = " MEI-F0122 UPON CONS
*****         DISPLAY "MEI-F0123 = " MEI-F0123 UPON CONS
*****END-IF.
     READ     HMEIMS
              INVALID
              MOVE     "INV"   TO   HMEIMS-INV-FLG
              NOT INVALID
              MOVE     SPACE   TO   HMEIMS-INV-FLG
     END-READ.
*
 900-MEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    商品変換テーブル読込み                       *
*--------------------------------------------------------------*
 900-SHO-READ           SECTION.
*
     READ     HSHOTBL
              NEXT AT END
              MOVE     "END"           TO   END-FLG
              GO                       TO   900-SHO-READ-EXIT
     END-READ.
*件数カウント
     ADD      1           TO                READ-CNT.
*パラメタ取引先ＣＤ以上になったらＰＧ終了
     IF       SHO-F01  >  PARA-TORICD
              MOVE     "END"           TO   END-FLG
     END-IF.
*
 900-SHO-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
