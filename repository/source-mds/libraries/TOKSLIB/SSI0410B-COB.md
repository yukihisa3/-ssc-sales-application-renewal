# SSI0410B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSI0410B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　支払照合　箱根登山興業            *
*    モジュール名　　　　：　請求データ消し込み　　　　　　　　*
*    作成日／更新日　　　：　00/11/07                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払明細ファイルと請求明細ファイル*
*                        ：　の金額を照合し、一致したデータを　*
*                            削除する。                        *
*    作成日／更新日　　　：　08/08/28                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　内部統制対応　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI0410B.
 AUTHOR.                N.KANEKO.
 DATE-WRITTEN.          00/11/07.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払合計ファイル >>--*
     SELECT   SITGKFG   ASSIGN         DA-01-S-SITGKFG
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SSI-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   SETGKFK   ASSIGN         DA-01-VI-SETGKFK
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEI-F01   SEI-F05
                        STATUS         SEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  SITGKFG            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  1   RECORDS.
     COPY     SITGKFG   OF        XFDLIB
              JOINING   SSI       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  SETGKFK            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌﾟﾛｸﾞﾗﾑID >>--*
 01  PG-ID              PIC  X(08)     VALUE  "SSI0410B".
*----<< ﾌﾗｸﾞｴﾘｱ >>--*
 01  END-FLG            PIC  X(03)     VALUE  SPACE.
 01  INV-FLG            PIC  X(01)     VALUE  SPACE.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SSI-ST             PIC  X(02)     VALUE  SPACE.
 01  SEI-ST             PIC  X(02)     VALUE  SPACE.
*----<< ｶｳﾝﾄｴﾘｱ >>--*
 01  DEL-CNT            PIC  9(05)     VALUE  ZERO.
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
*----<< 伝票_変換（９桁用） >>--*
 01  WK-DENNO           PIC  9(11).
 01  WK-DENNO-R         REDEFINES     WK-DENNO.
     03  FILLER         PIC  X(02).
     03  HEN-DENNO      PIC  X(09).
*----<< 金額 >>--*
 01  WK-KINGAKU         PIC S9(08).
*
 LINKAGE                SECTION.
 01  PARA-OUT-CNT       PIC  9(07).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-OUT-CNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 SYO-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SITGKFG.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SITGKFGF  ERROR " SSI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 SEI-ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGKFK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SETGKFK   ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*プログラム開始メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*プログラムコントロール
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG  NOT =  SPACE.
     PERFORM  300-END-RTN.
*プログラム終了メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     INPUT     SITGKFG.
     OPEN     I-O       SETGKFK.
*照合ファイル初期ＲＥＡＤ
     PERFORM  900-SSI-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*取引先ｺｰﾄﾞ
**** MOVE     SSI-F02        TO   SEI-F01.
     MOVE     SSI-F02(3:4)   TO   SEI-F01.
*伝票番号
     MOVE     SSI-F09        TO   WK-DENNO.
     MOVE     HEN-DENNO      TO   SEI-F05.
     PERFORM  900-SEI-READ.
     IF       INV-FLG    =   SPACE
*             金額一致の場合削除
**************IF  WK-KINGAKU =   SEI-F06
                  DELETE  SETGKFK
                  ADD     1      TO   DEL-CNT
**************END-IF
     ELSE
              CONTINUE
     END-IF.
*
     PERFORM  900-SSI-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     CLOSE    SITGKFG.
     CLOSE    SETGKFK.
*削除件数表示
     DISPLAY "ｻｸｼﾞｮ ｹﾝｽｳ = " DEL-CNT UPON CONS.
*## 2008/08/28 内部統制対応
     MOVE     DEL-CNT    TO   PARA-OUT-CNT.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
     READ     SETGKFK
         INVALID
              MOVE      "E"           TO   INV-FLG
         NOT INVALID
              MOVE      SPACE         TO   INV-FLG
     END-READ.
 900-SEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払明細ファイル　READ                       *
*--------------------------------------------------------------*
 900-SSI-READ           SECTION.
     READ     SITGKFG
              AT END
              MOVE      "END"         TO   END-FLG
     END-READ.
     IF  SSI-F01  =     11
         IF   SSI-F06   =   SPACE
              MOVE SSI-F05            TO   WK-KINGAKU
         ELSE
              COMPUTE   WK-KINGAKU     =  SSI-F05 * -1
         END-IF
     ELSE
         IF   SSI-F15   =   SPACE
              MOVE SSI-F14            TO   WK-KINGAKU
         ELSE
              COMPUTE   WK-KINGAKU     =  SSI-F14 * -1
         END-IF
     END-IF.
 900-SSI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
