****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩＣ　　　　　　　　　　　　　*
*    モジュール名　　　　：　請求データ削除　　　　　　　　　　*
*    作成日／更新日　　　：　2015/09/03                        *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    処理概要　　　　　　：　不照合リストワークを参照して照合　*
*                        ：　ＯＫ分の請求合計ファイルを削除す　*
*                        ：　る。　　　　　　　　　　　　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SED0050B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/09/03.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 不照合リストワーク >>--*
     SELECT   EWFUSHF   ASSIGN         DA-01-VI-EWFUSHL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  FWK-F01
                                       FWK-F05
                                       FWK-F18
                                       FWK-F04
                        STATUS         FWK-STATUS.
*----<< 請求合計Ｆ >>--*
     SELECT   SETGKF    ASSIGN         DA-01-VI-SETGKF
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEI-F01
                                       SEI-F03
                                       SEI-F05
                        STATUS         SEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< EDIC不照合リストワーク >>--*
 FD  EWFUSHF            LABEL RECORD   IS   STANDARD.
     COPY     EWFUSHF   OF        XFDLIB
              JOINING   FWK       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  SETGKF             LABEL RECORD   IS   STANDARD.
     COPY     SETGKFP   OF        XFDLIB
              JOINING   SEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌﾟﾛｸﾞﾗﾑID >>--*
 01  PG-ID              PIC  X(08)     VALUE  "SED0050B".
*----<< ﾌﾗｸﾞｴﾘｱ >>--*
 01  END-FLG            PIC  X(03)     VALUE  SPACE.
 01  INV-FLG            PIC  X(01)     VALUE  SPACE.
*----<< INVALID FLG >>--*
 01  SEI-INVALID-FLG   PIC  X(01).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  FWK-STATUS         PIC  X(02)     VALUE  SPACE.
 01  SEI-ST             PIC  X(02)     VALUE  SPACE.
*----<< ｶｳﾝﾄｴﾘｱ >>--*
 01  READ-CNT           PIC  9(07)     VALUE  ZERO.
 01  DEL-CNT            PIC  9(07)     VALUE  ZERO.
 01  SKIP-CNT           PIC  9(07)     VALUE  ZERO.
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
 01  WK-SEIKIN          PIC  S9(11).
*----<< 店舗コード変換 >>--*
 01  TEMPOCD-MOJI           PIC  X(05).
 01  FILLER            REDEFINES      TEMPOCD-MOJI.
     03  TEMPOCD-SUCHI      PIC  9(05).
*----<< 伝票_変換（９桁用） >>--*
 01  WK-DENNO           PIC  9(09).
 01  WK-DENNO-R         REDEFINES     WK-DENNO.
     03  HEN-DENNO      PIC  X(09).
 LINKAGE                SECTION.
 01  PARA-OUT-CNT       PIC  9(07).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-OUT-CNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< EDIC不照合リストワーク >>--*
 EWFUSHF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      EWFUSHF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " EWFUSHL1  ERROR " FWK-STATUS " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 SEI-ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SETGKF    ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*
*----<< 開始メッセージ出力 >>--*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
*----<< メイン処理 >>--*
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG  NOT =  SPACE.
     PERFORM  300-END-RTN.
*
*----<< 終了メッセージ出力 >>--*
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
*
*----<<FILE OPEN >>--*
     OPEN     INPUT     EWFUSHF.
     OPEN     I-O       SETGKF.
*
*不照合リストワーク初期ＲＥＡＤ
     PERFORM  900-FWK-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
*----<< 照合ＯＫなら削除 >>--*
     IF  FWK-F32  =  "1"
*
*----<< 請求合計Ｆ読込 >>--*
         MOVE     FWK-F01            TO   SEI-F01
         MOVE     FWK-F05            TO   TEMPOCD-MOJI
         MOVE     TEMPOCD-SUCHI      TO   SEI-F03
         MOVE     FWK-F04(1:9)       TO   SEI-F05
         PERFORM  900-SEI-READ
*
*----<< 請求合計Ｆが存在したら削除 >>--*
         IF  SEI-INVALID-FLG  =  SPACE
             DELETE  SETGKF
             ADD     1      TO   DEL-CNT
         END-IF
     ELSE
         ADD      1                  TO   SKIP-CNT
     END-IF.
*
*不照合リストワーク読込み
     PERFORM  900-FWK-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
*----<<FILE CLOSE >>--*
     CLOSE    EWFUSHF.
     CLOSE    SETGKF.
*
*----<< 処理件数表示 >>--*
     DISPLAY "READ  CNT  = " READ-CNT UPON CONS.
     DISPLAY "SKIP  CNT  = " SKIP-CNT UPON CONS.
     DISPLAY "ｻｸｼﾞｮ ｹﾝｽｳ = " DEL-CNT UPON CONS.
*
*----<< ＯＵＴパラメーターセット >>--*
     MOVE   DEL-CNT      TO  PARA-OUT-CNT.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    不照合リストワーク        READ               *
*--------------------------------------------------------------*
 900-FWK-READ           SECTION.
*
 900-FWK-READ-010.
*
     READ     EWFUSHF   NEXT
              AT END
              MOVE      "END"         TO   END-FLG
              GO  TO     900-FWK-READ-EXIT
     END-READ.
*
     ADD     1  TO     READ-CNT.
*
 900-FWK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
*
*----<< 読み込み >>--*
     READ     SETGKF
       INVALID
           MOVE     "1"             TO   SEI-INVALID-FLG
       NOT INVALID
           MOVE      SPACE          TO   SEI-INVALID-FLG
     END-READ.
 900-SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
