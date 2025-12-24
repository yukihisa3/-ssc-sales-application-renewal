****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ＰＣ手書連携処理　　　　　　　　　*
*    モジュール名　　　　：　ＰＣ側手書取込量販店ＪＮＬ変換１　*
*    作成日／作成者　　　：　2009/10/27 NAV T.T                *
*　　　　　　　　　　　　：　量販店ＪＮＬを分割し、売上中間Ｆ・*
*　　　　　　　　　　　　　　備考ＪＮＬを作成する。　　　　　　*
*    更新日／更新者　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SPD0160B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*<<量販データＦ        >>*******
     SELECT      PCRYOJL4    ASSIGN    DA-01-VI-PCRYOJL4
                                       ORGANIZATION   INDEXED
                                       ACCESS    MODE SEQUENTIAL
                                       RECORD    KEY  RYO-F11
                                                      RYO-F011
                                                      RYO-F012
                                                      RYO-F02
                                       FILE STATUS    RYO-ST.
*<<伝票データＦ        >>*******
     SELECT      PCURIWK     ASSIGN    DA-01-S-PCURIWK
                                       FILE STATUS    URI-ST.
*<<量販店備考ワーク    >>*******
     SELECT      PCUBIKL1    ASSIGN    DA-01-VI-PCUBIKL1
                                       ORGANIZATION   INDEXED
                                       ACCESS    MODE RANDOM
                                       RECORD    KEY  BIK-F01
                                                      BIK-F02
                                                      BIK-F03
                                       FILE STATUS    BIK-ST.
*<< 取引先マスタ       >>*******
     SELECT      HTOKMS      ASSIGN    DA-01-VI-TOKMS2
                                       ORGANIZATION   INDEXED
                                       ACCESS    MODE RANDOM
                                       RECORD    KEY  TOK-F01
                                       FILE STATUS    TOK-ST.
*<< 店舗マスタ         >>*******
     SELECT      HTENMS     ASSIGN     DA-01-VI-TENMS1
                                       ORGANIZATION   INDEXED
                                       ACCESS    MODE DYNAMIC
                                       RECORD    KEY  TEN-F52
                                                      TEN-F011
                                       FILE STATUS    TEN-ST.
*<< 商品名称マスタ     >>*******
     SELECT   HMEIMS    ASSIGN         DA-01-VI-MEIMS1
                                       ORGANIZATION   INDEXED
                                       ACCESS    MODE DYNAMIC
                                       RECORD    KEY  MEI-F01
                                       FILE STATUS    MEI-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*<<量販データＦ        >>*******
 FD  PCRYOJL4
     LABEL       RECORD      IS        STANDARD.
     COPY        PCRYOJL4    OF        XFDLIB
     JOINING     RYO         AS        PREFIX.
*<<伝票データＦ        >>*******
 FD  PCURIWK
     LABEL       RECORD      IS        STANDARD.
     COPY        PCURIWK     OF        XFDLIB
     JOINING     URI         AS        PREFIX.
*<<量販店備考ワーク    >>*******
 FD  PCUBIKL1
     LABEL       RECORD      IS        STANDARD.
     COPY        PCUBIKL1    OF        XFDLIB
     JOINING     BIK         AS        PREFIX.
*<< 取引先マスタ       >>*******
 FD  HTOKMS
     LABEL       RECORD      IS        STANDARD.
     COPY        HTOKMS      OF        XFDLIB
     JOINING     TOK         AS        PREFIX.
*<< 店舗マスタ         >>*******
 FD  HTENMS
     LABEL       RECORD      IS        STANDARD.
     COPY        HTENMS      OF        XFDLIB
     JOINING     TEN         AS        PREFIX.
*<< 商品名称マスタ     >>*******
 FD  HMEIMS
     LABEL       RECORD      IS        STANDARD.
     COPY        HMEIMS      OF        XFDLIB
     JOINING     MEI         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  RYO-ST              PIC  X(02)  VALUE  SPACE.
     03  URI-ST              PIC  X(02)  VALUE  SPACE.
     03  TOK-ST              PIC  X(02)  VALUE  SPACE.
     03  TEN-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  BIK-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  I                   PIC  9(02)  VALUE  ZERO.
     03  READ-CNT            PIC  9(07)  VALUE  ZERO.
*
 01  FILE-ERR.
     03  RYO-ERR             PIC  N(10)  VALUE
                   NC"量販データＦ異常".
     03  URI-ERR             PIC  N(10)  VALUE
                   NC"伝票データＦ異常".
     03  TOK-ERR             PIC  N(10)  VALUE
                   NC"得意先マスタ異常".
     03  TEN-ERR             PIC  N(10)  VALUE
                   NC"店舗マスタ異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"商品名称マスタ異常".
     03  BIK-ERR             PIC  N(10)  VALUE
                   NC"量販店備考ワーク異常".
 LINKAGE                     SECTION.
 01  PARA-TOKCD              PIC  9(08).
 01  PARA-SMEMO              PIC  9(04).
 01  PARA-EMEMO              PIC  9(04).
*
 01  PARA-BUNCD              PIC  X(04).
 01  PARA-DENKU              PIC  X(02).
****************************************************************
 PROCEDURE                   DIVISION   USING  PARA-TOKCD
                                               PARA-SMEMO
                                               PARA-EMEMO
                                               PARA-BUNCD
                                               PARA-DENKU.
****************************************************************
 DECLARATIVES.
 RYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE PCRYOJL4.
     DISPLAY     RYO-ERR     UPON      STAT.
     DISPLAY     RYO-ST      UPON      STAT.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
 URI-ERR                      SECTION.
     USE         AFTER       EXCEPTION PROCEDURE PCURIWK.
     DISPLAY     URI-ERR     UPON      STAT.
     DISPLAY     URI-ST      UPON      STAT.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
*----<< 量販店備考ワーク >>--*
 BIK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE PCUBIKL1.
     DISPLAY     BIK-ERR     UPON      STAT.
     DISPLAY     BIK-ST      UPON      STAT.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR     UPON      STAT.
     DISPLAY     TOK-ST      UPON      STAT.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR     UPON      STAT.
     DISPLAY     TEN-ST      UPON      STAT.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
*----<< 商品名称マスタ >>--*
 HMEIMS-ERR             SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR     UPON      STAT.
     DISPLAY     MEI-ST      UPON      STAT.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  = "END".
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
*ファイルＯＰＥＮ
     OPEN        INPUT       PCRYOJL4  HTOKMS   HTENMS   HMEIMS
                 I-O         PCUBIKL1
                 OUTPUT      PCURIWK.
*量販店ＪＮＬスタート
     MOVE        SPACE       TO        RYO-REC.
     INITIALIZE                        RYO-REC.
*キーセット
     MOVE        PARA-TOKCD  TO        RYO-F11.
     MOVE        PARA-SMEMO  TO        RYO-F011.
     START       PCRYOJL4  KEY  IS >=  RYO-F11  RYO-F011
                                       RYO-F012 RYO-F02
                 INVALID
                 DISPLAY NC"＃＃対象ＤＴ無しＳＴ＃＃" UPON CONS
                 MOVE    4000          TO  PROGRAM-STATUS
                 STOP  RUN
     END-START.
*
     PERFORM  PCRYOJL4-READ-SEC.
     IF       END-FLG = "END"
              DISPLAY NC"＃＃対象ＤＴ無しＲＤ＃＃" UPON CONS
              MOVE    4000          TO  PROGRAM-STATUS
              STOP  RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 PCRYOJL4-READ-SEC           SECTION.
*量販データＲＥＡＤ
     READ     PCRYOJL4   NEXT   AT   END
              MOVE      "END"  TO   END-FLG
              GO               TO   PCRYOJL4-READ-EXIT
     END-READ.
*
     ADD      1                TO   READ-CNT.
*取引先ＣＤ判定
 READ010.
     IF       PARA-TOKCD  NOT =  RYO-F11
              MOVE      "END"  TO   END-FLG
              GO               TO   PCRYOJL4-READ-EXIT
     END-IF.
*エラー区分チェック
 READ020.
     IF       RYO-F574   =   "E"
              GO               TO   PCRYOJL4-READ-SEC
     END-IF.
*メモ_範囲チェック
 READ030.
     IF       PARA-EMEMO  <  RYO-F011
              MOVE      "END"  TO   END-FLG
     END-IF.
*
 PCRYOJL4-READ-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
*項目転送
     PERFORM  VARYING   I    FROM   1  BY   1   UNTIL   I  >  50
         IF  (RYO-F162(I)  NOT =  ZERO)
              PERFORM   TEN-SEC
         END-IF
     END-PERFORM.
*項目転送（摘要）
     IF  RYO-F61   NOT  =    ZERO
         PERFORM   VARYING   I  FROM  1  BY   1   UNTIL   I  >  50
              IF  (RYO-F162(I)  NOT =  ZERO)
                   PERFORM   TEN-SEC2
              END-IF
         END-PERFORM
     END-IF.
*
     PERFORM  PCRYOJL4-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*                 項 目 転 送
****************************************************************
 TEN-SEC                     SECTION.
*伝票ＲＥＣ初期化
     MOVE        SPACE       TO   URI-REC.
     INITIALIZE  URI-REC.
*
*****DISPLAY "URI-F277 1 = " URI-F277 UPON CONS.
     MOVE     RYO-F11        TO        URI-F01.
     MOVE     RYO-F011       TO        URI-F02.
     MOVE     RYO-F012       TO        URI-F03.
     MOVE     40             TO        URI-F051.
     MOVE     NC"売上伝票"   TO        URI-F052.
     MOVE     RYO-F03        TO        URI-F06.
     MOVE     RYO-F161(I)    TO        URI-F07.
     MOVE     RYO-F04        TO        URI-F08.
     MOVE     RYO-F05        TO        URI-F09.
*****MOVE     RYO-F63        TO        URI-F44.
     MOVE     RYO-F071       TO        URI-F111.
     MOVE     RYO-F072       TO        URI-F112.
*****MOVE     RYO-F072       TO        URI-F113.
*    MOVE     RYO-F08        TO        URI-F12.
*    UPD NAV OONO 2010/02/16
*    パラメタの分類コードが空白でない時更新
     IF PARA-BUNCD NOT = SPACE
        MOVE     PARA-BUNCD     TO        URI-F12
     END-IF.
*
     MOVE     RYO-F09        TO        URI-F131.
*    MOVE     RYO-F10        TO        URI-F132.
*    UPD NAV OONO 2010/02/16
*    パラメタの分類コードが空白でない時更新
     IF PARA-DENKU NOT = SPACE
        MOVE     PARA-DENKU     TO        URI-F132
     END-IF.
     MOVE     RYO-F60        TO        URI-F134.
     MOVE     RYO-F621       TO        URI-F1411.
     MOVE     RYO-F622       TO        URI-F1412.
     MOVE     RYO-F1211      TO        URI-F1421.
     MOVE     RYO-F1212      TO        URI-F1422.
     MOVE     RYO-F162(I)    TO        URI-F15.
     MOVE     RYO-F621       TO        MEI-F011.
     MOVE     RYO-F622       TO        MEI-F012.
     PERFORM  MEI-READ.
     MOVE       "1"            TO      URI-F16.
     MOVE     RYO-F131       TO        URI-F172.
     MOVE     RYO-F132       TO        URI-F173.
*
     COMPUTE  URI-F181 =    URI-F15  *    URI-F172.
     COMPUTE  URI-F182 =    URI-F15  *    URI-F173.
     MOVE     RYO-F15        TO        URI-F22.
     MOVE     RYO-F11        TO        TOK-F01.
     PERFORM  TOK-READ.
     MOVE     TOK-F52        TO        URI-F24.
     MOVE     RYO-F121       TO        URI-F25.
     MOVE     RYO-F56        TO        URI-F26.
*****MOVE     RYO-F57        TO        URI-F44.
     MOVE     TOK-F89        TO        URI-F276.
     MOVE     TOK-F89        TO        URI-F27B.
     MOVE     RYO-F58        TO        URI-F28.
     MOVE     RYO-F59        TO        URI-F29.
     MOVE     RYO-F11        TO        TEN-F52.
     MOVE     RYO-F161(I)    TO        TEN-F011.
     PERFORM  TEN-READ.
     MOVE     TEN-F04        TO        URI-F30.
     MOVE     RYO-F99        TO        URI-F99.
*****DISPLAY "URI-F277 2 = " URI-F277 UPON CONS.
*
     WRITE    URI-REC.
 TEN-EXIT.
     EXIT.
****************************************************************
*                 項 目 転 送
****************************************************************
 TEN-SEC2                    SECTION.
*量販店備考ワーク存在したら出力しない。
     MOVE        SPACE       TO   BIK-REC.
     INITIALIZE  BIK-REC.
     MOVE     RYO-F11        TO        BIK-F01.
     MOVE     RYO-F011       TO        BIK-F02.
     MOVE     80             TO        BIK-F03.
     READ     PCUBIKL1
         INVALID
              CONTINUE
         NOT INVALID
              GO   TO        TEN-EXIT2
     END-READ.
*
*伝票ＲＥＣ初期化
     MOVE        SPACE       TO   BIK-REC.
     INITIALIZE  BIK-REC.
*
     MOVE     RYO-F11        TO        BIK-F01.
     MOVE     RYO-F011       TO        BIK-F02.
     MOVE     80             TO        BIK-F03.
     MOVE     40             TO        BIK-F051.
     MOVE     NC"売上伝票"   TO        BIK-F052.
     MOVE     RYO-F03        TO        BIK-F06.
     MOVE     RYO-F161(I)    TO        BIK-F07.
     MOVE     RYO-F04        TO        BIK-F08.
     MOVE     RYO-F05        TO        BIK-F09.
*****MOVE     RYO-F63        TO        BIK-F44.
     MOVE     RYO-F071       TO        BIK-F111.
     MOVE     RYO-F072       TO        BIK-F112.
*****MOVE     RYO-F072       TO        BIK-F113.
     MOVE     RYO-F08        TO        BIK-F12.
*
     MOVE     RYO-F09        TO        BIK-F131.
     MOVE     RYO-F10        TO        BIK-F132.
     MOVE     RYO-F60        TO        BIK-F134.
     MOVE     RYO-F61        TO        BIK-F1411.
     MOVE     RYO-F141       TO        BIK-F1421.
     MOVE     RYO-F142       TO        BIK-F1422.
     MOVE     RYO-F11        TO        TOK-F01.
     PERFORM  TOK-READ.
     MOVE     TOK-F52        TO        BIK-F24.
     MOVE     RYO-F56        TO        BIK-F26.
*****MOVE     RYO-F57        TO        BIK-F44.
     MOVE     RYO-F58        TO        BIK-F28.
     MOVE     RYO-F59        TO        BIK-F29.
     MOVE     RYO-F11        TO        TEN-F52.
     MOVE     RYO-F161(I)    TO        TEN-F011.
     PERFORM  TEN-READ.
     MOVE     TEN-F04        TO        BIK-F30.
     MOVE     RYO-F99        TO        BIK-F99.
*
     WRITE    BIK-REC.
 TEN-EXIT2.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
     CLOSE    PCRYOJL4   HTOKMS   PCURIWK   HMEIMS  PCUBIKL1.
 END-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ　 READ
****************************************************************
 TOK-READ                  SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F52
     END-READ.
 TOK-READ-EXIT.
     EXIT.
****************************************************************
*    店舗マスタ　 READ
****************************************************************
 TEN-READ                  SECTION.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F04
     END-READ.
 TEN-READ-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ　 READ
****************************************************************
 MEI-READ                  SECTION.
     READ     HMEIMS    INVALID
              MOVE      ZERO           TO   MEI-F93
     END-READ.
 MEI-READ-EXIT.
     EXIT.
