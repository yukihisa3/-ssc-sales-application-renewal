****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ナフコ支払照合　　　　　　　　　　*
*    モジュール名　　　　：　照合データ消込　　　　　　　　　　*
*    作成日／更新日　　　：　10/10/07                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　照合したデータを請求明細ファイル　*
*                        ：　から削除する。　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY3774B.
 AUTHOR.                OONO.
 DATE-WRITTEN.          10/10/07.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 照合ファイル >>--*
     SELECT   NFSIHAF  ASSIGN         DA-01-VI-NFSIHAL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SIM-F01   SIM-F02
                                       SIM-F03   SIM-F04
                                       SIM-F05   SIM-F06
                                       SIM-F07   SIM-F08
                        STATUS         SIM-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   HSEIGKF   ASSIGN         DA-01-VI-SETGKFA2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEI-F01   SEI-F03
                                       SEI-F04   SEI-F05
                        STATUS         SEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 照合ファイル >>--*
 FD  NFSIHAF            LABEL RECORD   IS   STANDARD.
     COPY     NFSIHAF   OF        XFDLIB
              JOINING   SIM       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  HSEIGKF            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌﾟﾛｸﾞﾗﾑID >>--*
 01  PG-ID              PIC  X(08)     VALUE  "SSY3774B".
*----<< ﾌﾗｸﾞｴﾘｱ >>--*
 01  END-FLG            PIC  X(03)     VALUE  SPACE.
 01  INV-FLG            PIC  X(01)     VALUE  SPACE.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SIM-ST             PIC  X(02)     VALUE  SPACE.
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
*----<< 取引先変換（６桁用） >>--*
 01  WK-TORI            PIC  X(06).
 01  WK-TORI-R          REDEFINES     WK-TORI.
     03  HEN-TORI       PIC  9(06).
*----<< 伝票番号変更 >>--*
 01  WK-DENNO           PIC  9(09).
 01  WK-DENNO-R         REDEFINES     WK-DENNO.
     03  HEN-DENNO      PIC  X(09).
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
 SIM-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFSIHAF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " NFSIHAF  ERROR " SIM-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 SEI-ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSEIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " HSEIGKF   ERROR " SEI-ST " "
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
     OPEN     INPUT     NFSIHAF.
     OPEN     I-O       HSEIGKF.
*照合ファイル初期ＲＥＡＤ
     PERFORM  900-SIM-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*レコード区分＝空白の時（エラー区分）
     MOVE     SIM-F03        TO   SEI-F01.
     MOVE     SIM-F01        TO   SEI-F03.
     MOVE     SIM-F08        TO   SEI-F04.
     MOVE     SIM-F06        TO   WK-DENNO.
     MOVE     HEN-DENNO      TO   SEI-F05.
     PERFORM  900-SEI-READ.
     IF       INV-FLG    =   SPACE
              DELETE  HSEIGKF
              ADD     1      TO   DEL-CNT
     ELSE
              CONTINUE
     END-IF.
*
     PERFORM  900-SIM-READ.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     CLOSE    NFSIHAF.
     CLOSE    HSEIGKF.
*削除件数表示
     DISPLAY "ｻｸｼﾞｮ ｹﾝｽｳ = " DEL-CNT UPON CONS.
*内部統制対応
     MOVE     DEL-CNT        TO      PARA-OUT-CNT.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
     READ     HSEIGKF
         INVALID
              MOVE      "E"           TO   INV-FLG
         NOT INVALID
              MOVE      SPACE         TO   INV-FLG
     END-READ.
 900-SEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払明細Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-SIM-READ           SECTION.
     READ     NFSIHAF
              AT END
              MOVE      "END"         TO   END-FLG
     END-READ.
 900-SIM-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
